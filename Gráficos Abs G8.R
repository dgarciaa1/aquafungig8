library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(car)
library(lme4)
library(lmtest)

#### Cargamos el directorio de trabajo
setwd("C:/Users/david/OneDrive/Documentos/EAFIT/Tesis de grado/AQUAFUNGI G8/Datos y scripts de gráficos/")


### Pleurotus djamor (PLDJ) 2025

## Cargamos los datos de la biomasa para PLDJ 2025
df_pldj2025 <- read_excel("Gráficos Abs G8.xlsx", sheet = "PLDJ 2025")

## Transformamos las columnas categóricas a factores, y la ABS a numérica
df_pldj2025$Species <- factor(df_pldj2025$Species)
df_pldj2025$Treatment <- factor(df_pldj2025$Treatment)
df_pldj2025$`Time (days)` <- factor(df_pldj2025$`Time (days)`)
df_pldj2025$Replicate <- factor(df_pldj2025$Replicate)

## Creamos un dataframe con la absorbancia promedio 
## y errores estándares de PLDJ
df_pldj_bar2025 <- df_pldj2025 %>%
  group_by(Species, Treatment, `Time (days)`) %>%
  summarise(
    `Absorbance (630nm)` = mean(Value),
    se = sd(Value)/sqrt(n())
  )

## Graficamos puntos conectados en el tiempo de ABS de PLDJ 2025 usando gpplot

# Filtramos los tratamientos específicos, con sus respectivos controles:
  
df_pldj2025_3.5ph <- df_pldj_bar2025 %>%
  filter(Treatment %in% c("1", "4", "7", "Tap water", "Whey"))

df_pldj2025_4.8ph <- df_pldj_bar2025 %>%
  filter(Treatment %in% c("2", "5", "8", "Tap water", "Whey"))

df_pldj2025_6ph <- df_pldj_bar2025 %>%
  filter(Treatment %in% c("3", "6", "9", "Tap water", "Whey"))

# Primero, la gráfica de T1, T4 y T7. 
ggplot(df_pldj2025_3.5ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "1" = 21,  
    "4" = 22,  
    "7" = 23
  )) +
  scale_fill_manual(values = c(
    "1" = "darkgreen",
    "4" = "limegreen",
    "7" = "yellow3"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pleurotus djamor #1") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

# Segundo, la gráfica de T2, T5 y T8. 
ggplot(df_pldj2025_4.8ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "2" = 21,  
    "5" = 22,  
    "8" = 23
  )) +
  scale_fill_manual(values = c(
    "2" = "blue",
    "5" = "cyan3",
    "8" = "lightblue2"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pleurotus djamor #1") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

# Tercero, la gráfica de T3, T6 y T9. 
ggplot(df_pldj2025_6ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "3" = 21,  
    "6" = 22,  
    "9" = 23
  )) +
  scale_fill_manual(values = c(
    "3" = "darkred",
    "6" = "violet",
    "9" = "pink"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pleurotus djamor #1") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

## Cargamos un nuevo set de datos y transformamos las columnas para un LMM
df_pldj2025lmm <- read_excel("Gráficos Abs G8.xlsx", sheet = "PLDJ 2025 LMM")

df_pldj2025lmm$Species <- factor(df_pldj2025lmm$Species)
df_pldj2025lmm$Media <- factor(df_pldj2025lmm$Media)
df_pldj2025lmm$Treatment <- factor(df_pldj2025lmm$Treatment)
df_pldj2025lmm$pH <- factor(df_pldj2025lmm$pH)
df_pldj2025lmm$`Time (days)` <- as.numeric(df_pldj2025lmm$`Time (days)`)
df_pldj2025lmm$Replicate <- factor(df_pldj2025lmm$Replicate)
df_pldj2025lmm$Value <- as.numeric(df_pldj2025lmm$Value)

## Hacemos un modelo mixto lineal (LMM) del pH en el tiempo. 
pldj2025lmm <- lmer(Value ~ pH * Media * `Time (days)` + (1|Replicate), 
                     data = df_pldj2025lmm)
summary(pldj2025lmm)
Anova(pldj2025lmm, type = 3) 



### Pleurotus djamor (PLDJ) 2026-1

## Cargamos los datos de la biomasa para PLDJ 2026-1
df_pldj20261 <- read_excel("Gráficos Abs G8.xlsx", sheet = "PLDJ 2026-1")

## Transformamos las columnas categóricas a factores, y la ABS a numérica
df_pldj20261$Species <- factor(df_pldj20261$Species)
df_pldj20261$Treatment <- factor(df_pldj20261$Treatment)
df_pldj20261$`Time (days)` <- factor(df_pldj20261$`Time (days)`)
df_pldj20261$Replicate <- factor(df_pldj20261$Replicate)

## Creamos un dataframe con la absorbancia promedio 
## y errores estándares de PLDJ
df_pldj_bar20261 <- df_pldj20261 %>%
  group_by(Species, Treatment, `Time (days)`) %>%
  summarise(
    `Absorbance (630nm)` = mean(Value),
    se = sd(Value)/sqrt(n())
  )

## Graficamos puntos conectados en el tiempo de ABS de PLDJ 2026-1 usando gpplot

# Filtramos los tratamientos específicos, con sus respectivos controles:

df_pldj20261_3.5ph <- df_pldj_bar20261 %>%
  filter(Treatment %in% c("1", "4", "7", "Tap water", "Whey"))

df_pldj20261_4.8ph <- df_pldj_bar20261 %>%
  filter(Treatment %in% c("2", "5", "8", "Tap water", "Whey"))

df_pldj20261_6ph <- df_pldj_bar20261 %>%
  filter(Treatment %in% c("3", "6", "9", "Tap water", "Whey"))

# Primero, la gráfica de T1, T4 y T7. 
ggplot(df_pldj20261_3.5ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "1" = 21,  
    "4" = 22,  
    "7" = 23
  )) +
  scale_fill_manual(values = c(
    "1" = "darkgreen",
    "4" = "limegreen",
    "7" = "yellow3"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pleurotus djamor #2") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

# Segundo, la gráfica de T2, T5 y T8. 
ggplot(df_pldj20261_4.8ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "2" = 21,  
    "5" = 22,  
    "8" = 23
  )) +
  scale_fill_manual(values = c(
    "2" = "blue",
    "5" = "cyan3",
    "8" = "lightblue2"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pleurotus djamor #2") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

# Tercero, la gráfica de T3, T6 y T9. 
ggplot(df_pldj20261_6ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "3" = 21,  
    "6" = 22,  
    "9" = 23
  )) +
  scale_fill_manual(values = c(
    "3" = "darkred",
    "6" = "violet",
    "9" = "pink"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pleurotus djamor #2") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

## Cargamos un nuevo set de datos y transformamos las columnas para un LMM
df_pldj20261lmm <- read_excel("Gráficos Abs G8.xlsx", sheet = "PLDJ 2026-1 LMM")

df_pldj20261lmm$Species <- factor(df_pldj20261lmm$Species)
df_pldj20261lmm$Media <- factor(df_pldj20261lmm$Media)
df_pldj20261lmm$Treatment <- factor(df_pldj20261lmm$Treatment)
df_pldj20261lmm$pH <- factor(df_pldj20261lmm$pH)
df_pldj20261lmm$`Time (days)` <- as.numeric(df_pldj20261lmm$`Time (days)`)
df_pldj20261lmm$Replicate <- factor(df_pldj20261lmm$Replicate)
df_pldj20261lmm$Value <- as.numeric(df_pldj20261lmm$Value)

## Hacemos un modelo mixto lineal (LMM) del pH en el tiempo. 
pldj20261lmm <- lmer(Value ~ pH * Media * `Time (days)` + (1|Replicate), 
                     data = df_pldj20261lmm)
summary(pldj20261lmm)
Anova(pldj20261lmm, type = 3) 




### Pholiota adiposa (PHAD) 

## Cargamos los datos de la biomasa para PHAD
df_phad <- read_excel("Gráficos Abs G8.xlsx", sheet = "PHAD")

## Transformamos las columnas categóricas a factores, y la ABS a numérica
df_phad$Species <- factor(df_phad$Species)
df_phad$Treatment <- factor(df_phad$Treatment)
df_phad$`Time (days)` <- factor(df_phad$`Time (days)`)
df_phad$Replicate <- factor(df_phad$Replicate)

## Creamos un dataframe con la absorbancia promedio 
## y errores estándares de PHAD
df_phad_bar <- df_phad %>%
  group_by(Species, Treatment, `Time (days)`) %>%
  summarise(
    `Absorbance (630nm)` = mean(Value),
    se = sd(Value)/sqrt(n())
  )

## Graficamos puntos conectados en el tiempo de ABS de PHAD usando gpplot

# Filtramos los tratamientos específicos, con sus respectivos controles:

df_phad_3.5ph <- df_phad_bar %>%
  filter(Treatment %in% c("1", "4", "7", "Tap water", "Whey"))

df_phad_4.8ph <- df_phad_bar %>%
  filter(Treatment %in% c("2", "5", "8", "Tap water", "Whey"))

df_phad_6ph <- df_phad_bar %>%
  filter(Treatment %in% c("3", "6", "9", "Tap water", "Whey"))

# Primero, la gráfica de T1, T4 y T7. 
ggplot(df_phad_3.5ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "1" = 21,  
    "4" = 22,  
    "7" = 23
  )) +
  scale_fill_manual(values = c(
    "1" = "darkgreen",
    "4" = "limegreen",
    "7" = "yellow3"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pholiota adiposa") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

# Segundo, la gráfica de T2, T5 y T8. 
ggplot(df_phad_4.8ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "2" = 21,  
    "5" = 22,  
    "8" = 23
  )) +
  scale_fill_manual(values = c(
    "2" = "blue",
    "5" = "cyan3",
    "8" = "lightblue2"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pholiota adiposa") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

# Tercero, la gráfica de T3, T6 y T9. 
ggplot(df_phad_6ph, aes(x = `Time (days)`, y = `Absorbance (630nm)`)) +
  geom_line(aes(group = Treatment, color = Treatment)) +
  geom_point(aes(shape = Treatment, fill = Treatment),
             color = "black",
             size = 4,
             stroke = 0.8) +
  scale_shape_manual(values = c(
    "3" = 21,  
    "6" = 22,  
    "9" = 23
  )) +
  scale_fill_manual(values = c(
    "3" = "darkred",
    "6" = "violet",
    "9" = "pink"
  )) +
  scale_color_manual(values = c(
    "1" = "darkgreen",
    "2" = "blue",
    "3" = "darkred",
    "4" = "limegreen",  
    "5" = "cyan3",  
    "6" = "violet",  
    "7" = "yellow3",  
    "8" = "lightblue2",   
    "9" = "pink",
    "Whey" = "red",
    "Tap water" = "skyblue1"
  )) +
  geom_errorbar(aes(ymin = `Absorbance (630nm)` - se, 
                    ymax = `Absorbance (630nm)` + se, color = Treatment), 
                width = 0.1,
                linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.4)) +
  labs(title = "Pholiota adiposa") +
  theme(plot.title = element_text(size = 16, face = "italic"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 14.5),
        axis.text.y = element_text(size = 14.5))

## Cargamos un nuevo set de datos y transformamos las columnas para un LMM
df_phadlmm <- read_excel("Gráficos Abs G8.xlsx", sheet = "PHAD LMM")

df_phadlmm$Species <- factor(df_phadlmm$Species)
df_phadlmm$Media <- factor(df_phadlmm$Media)
df_phadlmm$Treatment <- factor(df_phadlmm$Treatment)
df_phadlmm$pH <- factor(df_phadlmm$pH)
df_phadlmm$`Time (days)` <- as.numeric(df_phadlmm$`Time (days)`)
df_phadlmm$Replicate <- factor(df_phadlmm$Replicate)
df_phadlmm$Value <- as.numeric(df_phadlmm$Value)

## Hacemos un modelo mixto lineal (LMM) del pH en el tiempo. 
phadlmm <- lmer(Value ~ pH * Media * `Time (days)` + (1|Replicate), 
                data = df_phadlmm)
summary(phadlmm)
Anova(phadlmm, type = 3) 
