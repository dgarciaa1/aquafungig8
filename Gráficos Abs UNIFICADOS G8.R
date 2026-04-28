library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(lme4)
library(lmtest)

#### Cargamos el directorio de trabajo
setwd("C:/Users/david/OneDrive/Documentos/EAFIT/Tesis de grado/AQUAFUNGI G8/Datos y scripts de gráficos/")


### Unificados para T9 pH, todos los hongos

## Cargamos los datos de pH T9 de todos los hongos 
df_abs_unificado <- read_excel("Gráficos Abs G8.xlsx", sheet = "UNIFICADOS")

## Transformamos las columnas categóricas a factores, y el pH a numérica
df_abs_unificado$Species <- factor(df_abs_unificado$Species)
df_abs_unificado$Treatment <- factor(df_abs_unificado$Treatment)
df_abs_unificado$`Time (days)` <- factor(df_abs_unificado$`Time (days)`)
df_abs_unificado$Replicate <- factor(df_abs_unificado$Replicate)

## Creamos un dataframe con errores estándares
df_abs_unificado_bar <- df_abs_unificado %>%
  group_by(Species, Treatment, `Time (days)`) %>%
  summarise(
    `Absorbance (630nm)` = mean(Value),
    se = sd(Value)/sqrt(n())
  )

## Graficamos puntos conectados en el tiempo de pH usando gpplot
ggplot(df_abs_unificado_bar, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Species, color = Species)) +
  geom_point(aes(shape = Species, fill = Species),
             color = "black",
             size = 4,
             stroke = 0.8) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = "black"), 
                width = 0.8,
                linewidth = 0.6) +
  scale_shape_manual(values = c(
    "Pholiota adiposa" = 22,  
    "Pleurotus djamor" = 23,
    "Pleurotus eryngii" = 24,
    "Pycnoporus sanguineus" = 25
  )) +
  scale_fill_manual(values = c(
    "Pholiota adiposa" = "cyan2",  
    "Pleurotus djamor" = "pink2",
    "Pleurotus eryngii" = "orange2",
    "Pycnoporus sanguineus" = "violet",
    "Whey" = "red",
    "Tap water" = "skyblue"
  )) +
  scale_color_manual(values = c(
    "Pholiota adiposa" = "cyan2",  
    "Pleurotus djamor" = "pink2",
    "Pleurotus eryngii" = "orange2",
    "Pycnoporus sanguineus" = "violet",
    "Whey" = "red",
    "Tap water" = "skyblue"
  )) +
  scale_y_continuous(breaks = seq(0, 14, by = 0.4)) +
  theme(plot.title = element_text(size = 16, face = "italic"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))
