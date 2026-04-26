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

### Pleurotus eryngii #2 (PLER)

## Cargamos los datos de la biomasa para PLER
df_pler2 <- read_excel("ANOVA Biomasa G8.xlsx", sheet = "PLER 2026-2")

## Transformamos las columnas categóricas a factores, y la biomasa a numérica
df_pler2$Species <- factor(df_pler2$Species)
df_pler2$Media <- factor(df_pler2$Media)
df_pler2$Treatment <- factor(df_pler2$Treatment)
df_pler2$pH <- factor(df_pler2$pH)
df_pler2$Replicate <- factor(df_pler2$Replicate)

## ANOVA de UN factor para PLER
aov_pler2 <- aov(Biomass ~ Media, data = df_pler2)
summary(aov_pler2)

## Post-hoc emmeans para ver si hay diferencias significativas entre tratamientos
emmeans(aov_pler2, pairwise ~ Media, 
        adjust = "tukey")

## Función para extraer p-values de ANOVA de PLER
smry_pler2 <- summary(aov_pler2)[[1]]

pval_pler2 <- data.frame(
  Factor = rownames(smry_pler2),
  p.value = smry_pler2[,"Pr(>F)"],
  row.names = NULL
)

## Normalidad en los errores - Como el tamaño de muestra es menor a n = 20,
## lo normal es que no haya normalidad en los residuales
shapiro.test(residuals(aov_pler2))

## Homocedasticidad
leveneTest(Biomass ~ Media * pH, data = df_pler2) 

## Eta cuadrados para ver la significancia biológica del pH, concentración y
## la interacción de ambos sobre el crecimiento de biomasa
eta_squared(aov_pler2, partial = TRUE, ci = 0.95)

## Creamos un dataframe con la biomasa promedio 
## y errores estándares de PLER
df_pler2_bar <- df_pler2 %>%
  group_by(Species,Treatment, Media, pH) %>%
  summarise(
    mean_biomass = mean(Biomass),
    se = sd(Biomass)/sqrt(n())
  )

## Graficamos barras de biomasa de PLER usando gpplot
ggplot(df_pler2_bar, aes(x = Media, y = mean_biomass, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(
    "T3" = "darkred",  
    "T6" = "violet",  
    "T9" = "pink"
  )) +
  geom_errorbar(aes(ymin = mean_biomass - se, ymax = mean_biomass + se),
                width = 0.2, position = position_dodge(0.9)) +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5)) +
  labs(title = "Pleurotus eryngii", y = "Biomass (g)", 
       x = "Whey concentration index",
       fill = "Treatment") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))
