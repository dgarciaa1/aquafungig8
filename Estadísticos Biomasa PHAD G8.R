library(readxl)
library(ggplot2)
library(dplyr)
library(car)
library(effectsize)
library(lme4)
library(lmtest)
library(emmeans)

#### Cargamos el directorio de trabajo
setwd("C:/Users/david/OneDrive/Documentos/EAFIT/Tesis de grado/AQUAFUNGI G8/Datos y scripts de gráficos/")

### Pholiota adiposa (PHAD)

## Cargamos los datos de la biomasa para PLDJ
df_phad <- read_excel("ANOVA Biomasa G8.xlsx", sheet = "PHAD")

## Transformamos las columnas categóricas a factores, y la biomasa a numérica
df_phad$Species <- factor(df_phad$Species)
df_phad$Media <- factor(df_phad$Media)
df_phad$Treatment <- factor(df_phad$Treatment)
df_phad$pH <- factor(df_phad$pH)
df_phad$Replicate <- factor(df_phad$Replicate)

## ANOVA de dos factores para PLDJ
aov_phad <- aov(Biomass ~ Media * pH, data = df_phad)
summary(aov_phad)

## Post-hoc emmeans para ver si hay diferencias significativas entre tratamientos
emmeans(aov_phad, pairwise ~ Media | pH, 
        adjust = "tukey")

## Función para extraer p-values de ANOVA de PLDJ
smry_phad <- summary(aov_phad)[[1]]

pval_phad <- data.frame(
  Factor = rownames(smry_phad),
  p.value = smry_phad[,"Pr(>F)"],
  row.names = NULL
)

## Normalidad en los errores
shapiro.test(residuals(aov_phad))

## Homocedasticidad
leveneTest(Biomass ~ Media * pH, data = df_phad) 

## Eta cuadrados para ver la significancia biológica del pH, concentración y
## la interacción de ambos sobre el crecimiento de biomasa
eta_squared(aov_phad, partial = TRUE, ci = 0.95)

## Creamos un dataframe con la biomasa promedio 
## y errores estándares de PLDJ
df_phad_bar <- df_phad %>%
  group_by(Species,Treatment, Media, pH) %>%
  summarise(
    mean_biomass = mean(Biomass),
    se = sd(Biomass)/sqrt(n())
  )

## Graficamos barras de biomasa de PLDJ usando gpplot
ggplot(df_phad_bar, aes(x = Media, y = mean_biomass, fill = pH)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(
    "2.8" = "violetred",  
    "4.3" = "darkorange",  
    "5.1" = "cyan3"
  )) +
  geom_errorbar(aes(ymin = mean_biomass - se, ymax = mean_biomass + se),
                width = 0.2, position = position_dodge(0.9)) +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5)) +
  labs(title = "Pholiota adiposa", y = "Mean Biomass (g)", 
       x = "Whey concentration index",
       fill = "Initial pH") +
  theme(plot.title = element_text(face = "italic"))
