library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(lme4)
library(lmtest)
library(scales)

#### Cargamos el directorio de trabajo
setwd("C:/Users/david/OneDrive/Documentos/EAFIT/Tesis de grado/AQUAFUNGI G8/Datos y scripts de gráficos/")


### DQO de todos los hongos

## Cargamos los datos de la DQO para todos los hongos
df_unificados <- read_excel("Gráficos DQO G8.xlsx", sheet = "UNIFICADOS")

## Transformamos las columnas categóricas a factores, y el pH a numérica
df_unificados$Species <- factor(df_unificados$Species)
df_unificados$Treatment <- factor(df_unificados$Treatment)
df_unificados$`Time (days)` <- factor(df_unificados$`Time (days)`)
df_unificados$Replicate <- factor(df_unificados$Replicate)

## Creamos un dataframe con errores estándares de los hongos (si tenían réplicas)
df_unificados <- df_unificados %>%
  group_by(Species, Treatment, `Time (days)`) %>%
  summarise(
    COD = mean(Value),
    se = sd(Value)/sqrt(n())
  )

## Graficamos puntos conectados en el tiempo de pH usando gpplot
ggplot(df_unificados, aes(x = `Time (days)`, y = COD)) +
  geom_line(aes(group = Species, color = Species)) +
  geom_point(aes(shape = Species, fill = Species),
             color = "black",
             size = 4,
             stroke = 0.8) +
  geom_errorbar(aes(ymin = COD - se, ymax = COD + se, color = "black"), 
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
    "Whey" = "red"
  )) +
  scale_color_manual(values = c(
    "Pholiota adiposa" = "cyan2",  
    "Pleurotus djamor" = "pink2",
    "Pleurotus eryngii" = "orange2",
    "Pycnoporus sanguineus" = "violet",
    "Whey" = "red"
  )) +
  scale_y_continuous(breaks = seq(22000, 65000, by = 5000),
                     labels = label_number(scale = 1e-4, suffix = )) +
  labs(y = "COD (mg L⁻¹) x 10⁴") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))
