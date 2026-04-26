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

### Pycnoporus sanguineus (PYSA)

## Cargamos los datos de la biomasa para PYSA
df_pysa <- read_excel("ANOVA Biomasa G8.xlsx", sheet = "PYSA")

## Transformamos las columnas categóricas a factores, y la biomasa a numérica
df_pysa$Species <- factor(df_pysa$Species)
df_pysa$Media <- factor(df_pysa$Media)
df_pysa$Treatment <- factor(df_pysa$Treatment)
df_pysa$pH <- factor(df_pysa$pH)
df_pysa$Replicate <- factor(df_pysa$Replicate)

## ANOVA de UN factor para PYSA
aov_pysa <- aov(Biomass ~ Media, data = df_pysa)
summary(aov_pysa)

## Post-hoc emmeans para ver si hay diferencias significativas entre tratamientos
emmeans(aov_pysa, pairwise ~ Media, 
        adjust = "tukey")

## Función para extraer p-values de ANOVA de PLER
smry_pysa <- summary(aov_pysa)[[1]]

pval_pysa <- data.frame(
  Factor = rownames(smry_pysa),
  p.value = smry_pysa[,"Pr(>F)"],
  row.names = NULL
)

## Normalidad en los errores - Como el tamaño de muestra es menor a n = 20,
## lo normal es que no haya normalidad en los residuales
shapiro.test(residuals(aov_pysa))

## Homocedasticidad
leveneTest(Biomass ~ Media * pH, data = df_pysa) 

## Eta cuadrados para ver la significancia biológica del pH, concentración y
## la interacción de ambos sobre el crecimiento de biomasa
eta_squared(aov_pysa, partial = TRUE, ci = 0.95)

## Creamos un dataframe con la biomasa promedio 
## y errores estándares de PYSA
df_pysa_bar <- df_pysa %>%
  group_by(Species,Treatment, Media, pH) %>%
  summarise(
    mean_biomass = mean(Biomass),
    se = sd(Biomass)/sqrt(n())
  )

## Graficamos barras de biomasa de PYSA usando gpplot
ggplot(df_pysa_bar, aes(x = Media, y = mean_biomass, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c(
    "T3" = "darkred",  
    "T6" = "violet",  
    "T9" = "pink"
  )) +
  geom_errorbar(aes(ymin = mean_biomass - se, ymax = mean_biomass + se),
                width = 0.2, position = position_dodge(0.9)) +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5)) +
  labs(title = "Pycnoporus sanguineus", y = "Biomass (g)", 
       x = "Whey concentration index",
       fill = "Treatment") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))
