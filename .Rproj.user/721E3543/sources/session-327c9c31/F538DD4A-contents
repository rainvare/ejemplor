# ============================
# Semana 1 - Un vistazo a los datos
# ============================

# Carga de paquetes
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)

# Ejemplo: cargar un dataset preliminar (puedes cambiarlo por uno real)
dataset <- read_csv("tips.csv")

# Vista inicial
glimpse(dataset)
summary(dataset)
head(dataset)

# Limpiar nombres de columnas
dataset <- clean_names(dataset)

# Revisar valores únicos por columna
dataset %>% summarise(across(everything(), ~n_distinct(.)))

# Verificar NAs
colSums(is.na(dataset))

# ============================
# Semana 2 - Definición del problema
# ============================

# Actividad:
# 1. Elegir un nuevo dataset de interés (Kaggle, datos abiertos).
# 2. Reemplazar el link anterior por su dataset seleccionado.
# 3. Formular 2–3 preguntas de análisis. Ejemplo:

# Preguntas de ejemplo:
# - ¿Cuál es el gasto promedio por tipo de cliente?
# - ¿Los días de mayor propina están relacionados con el sexo del cliente?
# - ¿Hay diferencias en las cuentas según el día de la semana?

# Guardar las preguntas en un archivo aparte o comentarlas aquí:

# preguntas <- c(
#   "¿...?",
#   "¿...?"
# )

# ============================
# Semana 3 - Análisis y visualización
# ============================

# Transformaciones necesarias
dataset <- dataset %>%
  mutate(
    tip_percentage = tip / total_bill * 100,
    day = as.factor(day),
    sex = as.factor(sex)
  )

# Visualización 1: distribución del gasto
ggplot(dataset, aes(x = total_bill)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribución del total de la cuenta", x = "Total Bill")

# Visualización 2: propinas por sexo
ggplot(dataset, aes(x = sex, y = tip_percentage)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Propinas (%) por sexo", y = "Propina %")

# Visualización 3: gasto promedio por día
dataset %>%
  group_by(day) %>%
  summarise(promedio = mean(total_bill)) %>%
  ggplot(aes(x = day, y = promedio)) +
  geom_col(fill = "steelblue") +
  labs(title = "Gasto promedio por día", y = "Total promedio")

# ============================
# Semana 4 - Presentación final
# ============================

# 1. Crear archivo RMarkdown con el análisis y visualizaciones
# 2. Publicar en GitHub
# 3. Compartir enlace en portafolio profesional

# Puedes usar este template base de RMarkdown:
# https://rmarkdown.rstudio.com/lesson-2.html

# Recomendación:
# knit a HTML o PDF -> subir a GitHub -> agregar descripción del proyecto en README

# BONUS: incluye reflexión final sobre hallazgos y posibles aplicaciones de tu análisis
