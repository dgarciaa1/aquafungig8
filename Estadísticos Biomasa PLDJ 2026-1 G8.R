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

### Pleurotus djamor (PLDJ) 2026-1

## Cargamos los datos de la biomasa para PLDJ
df_pldj20261 <- read_excel("ANOVA Biomasa G8.xlsx", sheet = "PLDJ 2026-1")

## Transformamos las columnas categóricas a factores, y la biomasa a numérica
df_pldj20261$Species <- factor(df_pldj20261$Species)
df_pldj20261$Media <- factor(df_pldj20261$Media)
df_pldj20261$Treatment <- factor(df_pldj20261$Treatment)
df_pldj20261$pH <- factor(df_pldj20261$pH)
df_pldj20261$Replicate <- factor(df_pldj20261$Replicate)

## ANOVA de dos factores para PLDJ
aov_pldj20261 <- aov(Biomass ~ Media * pH, data = df_pldj20261)
summary(aov_pldj20261)

## Post-hoc emmeans para ver si hay diferencias significativas entre tratamientos
emmeans(aov_pldj20261, pairwise ~ Media | pH, 
                            adjust = "tukey")

## Función para extraer p-values de ANOVA de PLDJ
smry_pldj20261 <- summary(aov_pldj20261)[[1]]

pval_pldj20261 <- data.frame(
  Factor = rownames(smry_pldj20261),
  p.value = smry_pldj20261[,"Pr(>F)"],
  row.names = NULL
)

## Normalidad en los errores
shapiro.test(residuals(aov_pldj20261))

## Homocedasticidad
leveneTest(Biomass ~ Media * pH, data = df_pldj20261) 

## Eta cuadrados para ver la significancia biológica del pH, concentración y
## la interacción de ambos sobre el crecimiento de biomasa
eta_squared(aov_pldj20261, partial = TRUE, ci = 0.95)

## Creamos un dataframe con la biomasa promedio 
## y errores estándares de PLDJ
df_pldj_bar20261 <- df_pldj20261 %>%
  group_by(Species,Treatment, Media, pH) %>%
  summarise(
    mean_biomass = mean(Biomass),
    se = sd(Biomass)/sqrt(n())
  )

## Graficamos barras de biomasa de PLDJ usando gpplot
ggplot(df_pldj_bar20261, aes(x = Media, y = mean_biomass, fill = pH)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(
    "3.5" = "violetred",  
    "4.8" = "darkorange",  
    "6" = "cyan3"
  )) +
  geom_errorbar(aes(ymin = mean_biomass - se, ymax = mean_biomass + se),
                width = 0.2, position = position_dodge(0.9)) +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5)) +
  labs(title = "Pleurotus djamor #2", y = "Mean Biomass (g)", 
       x = "Whey concentration index",
       fill = "Initial pH") +
  theme(plot.title = element_text(face = "italic"))

