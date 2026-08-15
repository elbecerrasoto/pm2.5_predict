# ==============================================================================
# PASO 1. IMPORTAR BIBLIOTECAS NECESARIAS
# ==============================================================================
library(tidyverse) # Manipulación de datos y gráficas (dplyr, ggplot2, lubridate)
library(readxl) # Lectura de archivos Excel
library(prophet) # Modelado de series de tiempo
library(writexl) # Exportación a formato Excel para stakeholders
library(future) # Configuración de procesamiento paralelo
library(furrr) # Aplicación de funciones en paralelo (purrr + future)

# ==============================================================================
# PASO 2. VARIABLES GLOBALES Y DE CONFIGURACIÓN
# ==============================================================================
# Archivos y Directorios
ARCHIVO_DATOS <- "datos/Tijuana.xlsx"
DIR_RESULTADOS <- "resultados"

# Fechas y Parámetros de Predicción
PROYECCION_INICIO <- ymd("2022-01-01")
PROYECCION_FINAL <- ymd("2022-12-31")
INTERVALO_CONFIANZA <- 0.95 # Nivel de confianza para yhat_lower y yhat_upper

# Bandera de Ejecución
REALIZAR_CV <- FALSE # Cambiar a FALSE para omitir Grid Search si ya conoces los parámetros

# Hiperparámetros de Prophet (Ahora como listas para Grid Search)
CP_N <- c(25, 10) # Número máximo de puntos de cambio a probar
CP_RANGE <- 0.8 # Proporción del histórico donde se permiten cambios
CP_PRIOR_SCALES <- c(0.01, 0.05, 0.1) # Escalas de flexibilidad a probar

# Rutas de salida para los archivos generados
SALIDA_XLSX <- file.path(DIR_RESULTADOS, "PM2.5_TJ_resultados.xlsx")
SALIDA_MODELO <- file.path(DIR_RESULTADOS, "PM2.5_TJ_modelo.Rds")
GRAFICA_RESIDUALES <- file.path(DIR_RESULTADOS, "PM2.5_TJ_residuales.svg")
GRAFICA_PRINCIPAL <- file.path(DIR_RESULTADOS, "PM2.5_TJ_prediccion.svg")
GRAFICA_COMPONENTES <- file.path(DIR_RESULTADOS, "PM2.5_TJ_componentes.png")
GRAFICA_CV_METRICAS <- file.path(DIR_RESULTADOS, "PM2.5_TJ_cv_metricas.png")

# ==============================================================================
# PASO 3. PREPARACIÓN DE DIRECTORIO Y DATOS
# ==============================================================================
# Crear el directorio de resultados si no existe
if (!dir.exists(DIR_RESULTADOS)) {
  dir.create(DIR_RESULTADOS, recursive = TRUE, showWarnings = FALSE)
}

# Cargar tabla, limpiar y aplicar transformación logarítmica
TJ <- read_excel(ARCHIVO_DATOS) |>
  mutate(
    FECHA = as_date(FECHA),
    PM2.5 = as.numeric(PM2.5)
  ) |>
  drop_na(FECHA, PM2.5) # Limpiar valores nulos

TJ_prophet <- TJ |>
  rename(ds = FECHA, y_original = PM2.5) |>
  # Transformación LOG para asegurar que no haya predicciones negativas.
  mutate(y = log(y_original + 1e-9))





# ==============================================================================
# PASO 3. PREPARACIÓN DE DIRECTORIO, DATOS Y ASERCIONES (ASSERTIONS)
# ==============================================================================
# Crear el directorio de resultados si no existe
if (!dir.exists(DIR_RESULTADOS)) {
  dir.create(DIR_RESULTADOS, recursive = TRUE, showWarnings = FALSE)
}

# Cargar tabla
TJ <- read_excel(ARCHIVO_DATOS)

# Aserciones: Garantizar que la tabla tiene la estructura correcta
stopifnot(
  "ERROR: La tabla debe contener las columnas exactas 'FECHA' y 'PM2.5'" =
    all(c("FECHA", "PM2.5") %in% colnames(TJ))
)

# Aserciones: Garantizar que no hay NAs después de la limpieza
stopifnot(
  "ERROR: Se detectaron valores faltantes (NA)" =
    !any(is.na(TJ$FECHA)) && !any(is.na(TJ$PM2.5))
)

# Limpiar y castear tipos de datos
TJ <- TJ |>
  mutate(
    FECHA = as_date(FECHA),
    PM2.5 = as.numeric(PM2.5)
  ) # |> drop_na(FECHA, PM2.5)


TJ_prophet <- TJ |>
  rename(ds = FECHA, y_original = PM2.5) |>
  # Transformación LOG para asegurar que no haya predicciones negativas.
  mutate(y = log(y_original + 1e-9))

# Separación (Split) de datos: Entrenamiento y Prueba (OOS)
fecha_corte <- max(TJ_prophet$ds) - days(DIAS_PRUEBA)
TJ_train <- TJ_prophet |> filter(ds <= fecha_corte)
TJ_test <- TJ_prophet |> filter(ds > fecha_corte)

cat(sprintf(
  "Datos divididos: %d filas de entrenamiento, %d filas para prueba (OOS)\n",
  nrow(TJ_train), nrow(TJ_test)
))









# Calcular días futuros y predecir
dias_futuros <- as.numeric(PROYECCION_FINAL - max(TJ_prophet$ds))
futuro <- make_future_dataframe(modelo_prophet, periods = dias_futuros, freq = "day")

resultados_log <- predict(modelo_prophet, futuro) |> as_tibble()

# Revertir la transformación logarítmica (exp)
resultados <- resultados_log |>
  mutate(
    ds = as_date(ds),
    yhat = exp(yhat),
    yhat_lower = exp(yhat_lower),
    yhat_upper = exp(yhat_upper)
  )

# quitar dias festivos
names(resultados)

resultados <- resultados |>
  select(
    -(`Ano Nuevo [New Year's Day]`:`Dia del Trabajo [Labour Day] (Observed)_upper`), -(`Natalicio de Benito Juarez [Benito Juarez's birthday]`:`Navidad [Christmas] (Observed)_upper`),
    -starts_with("multiplicative")
  )

resultados <- resultados |>
  filter(ds > ymd("2021-12-31"))
puntos_cambio <- as_date(modelo_prophet$changepoints)

# ==============================================================================
# PASO 6. VISUALIZACIONES DE EVALUACIÓN (TROUBLESHOOTING)
# ==============================================================================
# 6.1 Gráfica de Residuales
evaluacion <- resultados |>
  select(ds, yhat) |>
  inner_join(TJ_prophet |> select(ds, y_original), by = "ds") |>
  mutate(residual = y_original - yhat)

grafica_residuales <- ggplot(evaluacion, aes(x = ds, y = residual)) +
  geom_point(alpha = 0.5, color = "#5D6D7E") +
  geom_hline(yintercept = 0, color = "#C0392B", linetype = "dashed", size = 1) +
  labs(
    title = "Análisis de Residuales (Real vs Predicción)",
    subtitle = "Valores más cercanos a la línea roja indican mejor predicción",
    x = "Fecha", y = "Error (PM2.5)"
  ) +
  theme_minimal()

ggsave(GRAFICA_RESIDUALES, plot = grafica_residuales, width = 10, height = 6)

# 6.2 Gráfica de Componentes (Tendencia y Estacionalidad desglosada)
png(GRAFICA_COMPONENTES, width = 800, height = 800, res = 100)
prophet_plot_components(modelo_prophet, resultados_log)
dev.off()

# ==============================================================================
# PASO 7. GRÁFICA PRINCIPAL (TIPO FIVETHIRTYEIGHT)
# ==============================================================================
tema_profesional <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#F0F0F0", color = NA),
    panel.background = element_rect(fill = "#F0F0F0", color = NA),
    panel.grid.major = element_line(color = "#D9D9D9", size = 0.6),
    panel.grid.minor = element_blank(),
    text = element_text(color = "#333333"),
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

grafica_principal <- ggplot() +
  geom_ribbon(
    data = resultados, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
    fill = "#B0BEC5", alpha = 0.4
  ) +
  geom_point(
    data = TJ_prophet, aes(x = ds, y = y_original),
    color = "#5D6D7E", size = 1.2, alpha = 0.5
  ) +
  geom_vline(
    xintercept = puntos_cambio,
    color = "#99A3A4", linetype = "dotted", size = 0.6, alpha = 0.8
  ) +
  geom_line(
    data = resultados, aes(x = ds, y = yhat),
    color = "#D81B60", size = 1
  ) +
  geom_line(
    data = TJ_prophet, aes(x = ds, y = y_original),
    color = "#D81B60", size = 1
  ) +
  geom_vline(
    xintercept = PROYECCION_INICIO,
    color = "#2C3E50", linetype = "dashed", size = 1
  ) +
  tema_profesional +
  labs(
    title = "Pronóstico de PM2.5 en Tijuana",
    subtitle = paste("Modelo log-transformado proyectado al", PROYECCION_FINAL, "(Las líneas punteadas indican cambios de tendencia)"),
    x = "Fecha",
    y = "Concentración de PM2.5 (µg/m³)"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months")

print(grafica_principal)
ggsave(GRAFICA_PRINCIPAL, plot = grafica_principal, width = 12, height = 7, dpi = 300)

# ==============================================================================
# PASO 8. GUARDAR DATOS TABULARES Y MODELO
# ==============================================================================
# Exportar estimaciones revertidas al formato natural en Excel
resultados |>
  select(ds, yhat, yhat_lower, yhat_upper, trend) |>
  write_xlsx(SALIDA_XLSX)

# Exportar el modelo ajustado en formato R (.Rds) para uso posterior
saveRDS(modelo_prophet, file = SALIDA_MODELO)
cat(sprintf("\n¡Proceso finalizado con éxito! El modelo se guardó en: %s\n", SALIDA_MODELO))
