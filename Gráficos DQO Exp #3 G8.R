library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(lme4)
library(lmtest)
library(scales)

#### Cargamos el directorio de trabajo
setwd("C:/Users/david/OneDrive/Documentos/EAFIT/Tesis de grado/AQUAFUNGI G8/Datos y scripts de gráficos/")


### Pleurotus eryngii (PLER) #2

## Cargamos los datos de la biomasa para PLER #2
df_pler2 <- read_excel("Gráficos DQO G8.xlsx", sheet = "PLER 2026-2")

## Transformamos las columnas categóricas a factores, y el pH a numérica
df_pler2$Species <- factor(df_pler2$Species)
df_pler2$Treatment <- factor(df_pler2$Treatment)
df_pler2$`Time (days)` <- factor(df_pler2$`Time (days)`)
df_pler2$Replicate <- factor(df_pler2$Replicate)

## Creamos un dataframe con errores estándares de PLER #2
df_pler2_bar <- df_pler2 %>%
  group_by(Species, Treatment, `Time (days)`) %>%
  summarise(
    COD = mean(Value),
    se = sd(Value)/sqrt(n())
  )

## Graficamos puntos conectados en el tiempo de pH de PLER #2 usando gpplot
ggplot(df_pler2_bar, aes(x = `Time (days)`, y = COD)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  geom_errorbar(aes(ymin = COD - se, ymax = COD + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_shape_manual(values = c(
    "6" = 22,  
    "9" = 23
  )) +
  scale_fill_manual(values = c(
    "6" = "violet",
    "9" = "pink2"
  )) +
  scale_color_manual(values = c(
    "6" = "violet",  
    "9" = "pink2",
    "Whey" = "red"
  )) +
  scale_y_continuous(breaks = seq(22000, 65000, by = 5000),
                     labels = label_number(scale = 1e-4, suffix = )) +
  labs(title = "Pleurotus eryngii", y = "COD (mg L⁻¹) x 10⁴") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

## Cargamos un nuevo set de datos y transformamos las columnas para un LM
df_pler2lm <- read_excel("Gráficos DQO G8.xlsx", sheet = "PLER 2026-2 LM")

df_pler2lm$Species <- factor(df_pler2lm$Species)
df_pler2lm$Media <- factor(df_pler2lm$Media)
df_pler2lm$Treatment <- factor(df_pler2lm$Treatment)
df_pler2lm$`Time (days)` <- as.numeric(df_pler2lm$`Time (days)`)
df_pler2lm$Replicate <- factor(df_pler2lm$Replicate)
df_pler2lm$Value <- as.numeric(df_pler2lm$Value)

## Hacemos un modelo lineal (LM) del pH en el tiempo. 
pler2lm_t6 <- lm(Value ~ `Time (days)`, data = subset(df_pler2lm, 
                                                      Treatment == "6"))
pler2lm_t9 <- lm(Value ~ `Time (days)`, data = subset(df_pler2lm, 
                                                      Treatment == "9"))

summary(pler2lm_t6)
summary(pler2lm_t9)




### Pycnoporus sanguineus (PYSA)

## Cargamos los datos de la biomasa para PYSA
df_pysa <- read_excel("Gráficos DQO G8.xlsx", sheet = "PYSA")

## Transformamos las columnas categóricas a factores, y el pH a numérica
df_pysa$Species <- factor(df_pysa$Species)
df_pysa$Treatment <- factor(df_pysa$Treatment)
df_pysa$`Time (days)` <- factor(df_pysa$`Time (days)`)
df_pysa$Replicate <- factor(df_pysa$Replicate)

## Creamos un dataframe con errores estándares de PYSA
df_pysa_bar <- df_pysa %>%
  group_by(Species, Treatment, `Time (days)`) %>%
  summarise(
    COD = mean(Value),
    se = sd(Value)/sqrt(n())
  )

## Graficamos puntos conectados en el tiempo de pH de PYSA usando gpplot
ggplot(df_pysa_bar, aes(x = `Time (days)`, y = COD)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  geom_errorbar(aes(ymin = COD - se, ymax = COD + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_shape_manual(values = c(
    "6" = 22,  
    "9" = 23
  )) +
  scale_fill_manual(values = c(
    "6" = "violet",
    "9" = "pink2"
  )) +
  scale_color_manual(values = c(
    "6" = "violet",  
    "9" = "pink2",
    "Whey" = "red"
  )) +
  scale_y_continuous(breaks = seq(16000, 65000, by = 5000),
                     labels = label_number(scale = 1e-4, suffix = )) +
  labs(title = "Pycnoporus sanguineus", y = "COD (mg L⁻¹) x 10⁴") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

## Cargamos un nuevo set de datos y transformamos las columnas para un LM
df_pysalm <- read_excel("Gráficos DQO G8.xlsx", sheet = "PYSA LM")

df_pysalm$Species <- factor(df_pysalm$Species)
df_pysalm$Media <- factor(df_pysalm$Media)
df_pysalm$Treatment <- factor(df_pysalm$Treatment)
df_pysalm$`Time (days)` <- as.numeric(df_pysalm$`Time (days)`)
df_pysalm$Replicate <- factor(df_pysalm$Replicate)
df_pysalm$Value <- as.numeric(df_pysalm$Value)

## Hacemos un modelo lineal (LM) del pH en el tiempo. 
pysalm_t6 <- lm(Value ~ `Time (days)`, data = subset(df_pysalm, 
                                                     Treatment == "6"))
pysalm_t9 <- lm(Value ~ `Time (days)`, data = subset(df_pysalm, 
                                                     Treatment == "9"))
summary(pysalm_t6)
summary(pysalm_t9)
