library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(lmtest)

#### Cargamos el directorio de trabajo
setwd("C:/Users/david/OneDrive/Documentos/EAFIT/Tesis de grado/AQUAFUNGI G8/Datos y scripts de gráficos/")


### Pleurotus djamor (PLDJ) 2025

## Cargamos los datos de la biomasa para PLDJ 2025
df_pldj2025 <- read_excel("Gráficos DQO G8.xlsx", sheet = "PLDJ 2025 LMM")

## Transformamos las columnas categóricas a factores
df_pldj2025$Species <- factor(df_pldj2025$Species)
df_pldj2025$Treatment <- factor(df_pldj2025$Treatment)
df_pldj2025$`Time (days)` <- factor(df_pldj2025$`Time (days)`)

## Graficamos puntos conectados en el tiempo de DQO de PLDJ 2025 usando gpplot
ggplot(df_pldj2025, aes(x = `Time (days)`, y = Value)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "9" = 23
  )) +
  scale_fill_manual(values = c(
    "9" = "pink3",
    "Whey" = "red"
  )) +
  scale_color_manual(values = c(
    "9" = "pink3",
    "Whey" = "red"
  )) +
  scale_y_continuous(breaks = seq(0, 100000, by = 200)) +
  labs(title = "Pleurotus djamor #1", x = "Time (days)", 
       y = "COD (mg O 2 L -1)") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

## Cargamos un nuevo set de datos y transformamos las columnas 
df_pldj2025lmm <- read_excel("Gráficos DQO G8.xlsx", sheet = "PLDJ 2025 LMM")

df_pldj2025lmm$Species <- factor(df_pldj2025lmm$Species)
df_pldj2025lmm$Media <- factor(df_pldj2025lmm$Media)
df_pldj2025lmm$Treatment <- factor(df_pldj2025lmm$Treatment)
df_pldj2025lmm$pH <- factor(df_pldj2025lmm$pH)
df_pldj2025lmm$`Time (days)` <- as.numeric(df_pldj2025lmm$`Time (days)`)
df_pldj2025lmm$Replicate <- factor(df_pldj2025lmm$Replicate)
df_pldj2025lmm$Value <- as.numeric(df_pldj2025lmm$Value)

## Hacemos un modelo de decaimiento exponencial
pldj2025lmm <- nls(Value ~ c + a * exp(-b * `Time (days)`), data = df_pldj2025lmm,
                   start = list(a = max(df_pldj2025lmm$Value), 
                                b = 0.01,
                                c = min(df_pldj2025lmm$Value)))
summary(pldj2025lmm)



### Pholiota adiposa (PHAD)

## Cargamos los datos de la biomasa para PHAD
df_phad <- read_excel("Gráficos DQO G8.xlsx", sheet = "PHAD")

## Transformamos las columnas categóricas a factores
df_phad$Species <- factor(df_phad$Species)
df_phad$Treatment <- factor(df_phad$Treatment)
df_phad$`Time (days)` <- factor(df_phad$`Time (days)`)

## Graficamos puntos conectados en el tiempo de DQO de PHAD usando gpplot
ggplot(df_phad, aes(x = `Time (days)`, y = Value)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "9" = 23
  )) +
  scale_fill_manual(values = c(
    "9" = "pink3",
    "Whey" = "red"
  )) +
  scale_color_manual(values = c(
    "9" = "pink3",
    "Whey" = "red"
  )) +
  scale_y_continuous(breaks = seq(0, 100000, by = 2000)) +
  labs(title = "Pholiota adiposa", x = "Time (days)", 
       y = "COD (mg O 2 L -1)") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

## Cargamos un nuevo set de datos y transformamos las columnas
df_phadlmm <- read_excel("Gráficos Abs G8.xlsx", sheet = "PHAD LMM")

df_phadlmm$Species <- factor(df_phadlmm$Species)
df_phadlmm$Media <- factor(df_phadlmm$Media)
df_phadlmm$Treatment <- factor(df_phadlmm$Treatment)
df_phadlmm$pH <- factor(df_phadlmm$pH)
df_phadlmm$`Time (days)` <- as.numeric(df_phadlmm$`Time (days)`)
df_phadlmm$Replicate <- factor(df_phadlmm$Replicate)
df_phadlmm$Value <- as.numeric(df_phadlmm$Value)

## Hacemos un modelo de decaimiento exponencial 
phadlmm <- nls(Value ~ c + a * exp(-b * `Time (days)`), data = df_phadlmm,
                   start = list(a = max(df_phadlmm$Value), 
                                b = 0.01,
                                c = min(df_phadlmm$Value)))
summary(phadlmm)

