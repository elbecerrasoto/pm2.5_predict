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
# PASO 4. VALIDACIÓN CRUZADA EN PARALELO
# ==============================================================================
if (REALIZAR_CV) {
  cat("Iniciando Validación Cruzada en PARALELO para elegir hiperparámetros...\n")

  # Cuadrícula (grid) incluyendo puntos de cambio (cp_n)
  grid_busqueda <- expand_grid(
    cp_n = CP_N,
    cp_prior = CP_PRIOR_SCALES,
    usar_festivos = c(FALSE, TRUE)
  )

  # Configurar el plan de paralelización (usando todos los núcleos disponibles menos 1)
  nucleos_disponibles <- parallelly::availableCores() - 1
  plan(multisession, workers = max(1, nucleos_disponibles))

  cat(sprintf(
    "Evaluando %d combinaciones usando %d núcleos. Por favor espera...\n",
    nrow(grid_busqueda), max(1, nucleos_disponibles)
  ))

  # future_pmap_dfr itera sobre el grid en paralelo y consolida los resultados en un DataFrame
  resultados_cv <- future_pmap_dfr(grid_busqueda, function(cp_n, cp_prior, usar_festivos) {
    # 1. Instanciar modelo temporal
    m_temp <- prophet(
      n.changepoints = cp_n,
      changepoint.range = CP_RANGE,
      changepoint.prior.scale = cp_prior
    )

    if (usar_festivos) {
      m_temp <- add_country_holidays(m_temp, country_name = "MX")
    }

    # Ajustar modelo suprimiendo salidas a consola por hilo
    m_temp <- fit.prophet(m_temp, TJ_prophet)

    # 2. Ejecutar Cross-Validation
    df_cv <- cross_validation(m_temp, initial = 365, period = 180, horizon = 90, units = "days")
    df_p <- performance_metrics(df_cv)
    rmse_promedio <- mean(df_p$rmse)

    # 3. Retornar los parámetros, su error, y anidar el dataframe del CV
    tibble(
      cp_n = cp_n,
      cp_prior = cp_prior,
      usar_festivos = usar_festivos,
      rmse = rmse_promedio,
      df_cv_data = list(df_cv) # Guardamos los datos completos como lista
    )
  }, .options = furrr_options(seed = TRUE))

  # Volver a procesamiento secuencial por buenas prácticas
  plan(sequential)

  # Encontrar el modelo con el menor RMSE
  mejor_resultado <- resultados_cv |>
    arrange(rmse) |>
    slice(1)
  mejor_modelo_params <- as.list(mejor_resultado)
  df_cv_final <- mejor_resultado$df_cv_data[[1]]

  cat(sprintf(
    ">>> Mejor modelo -> N Puntos: %d | Escala: %f | Festivos: %s | RMSE: %f\n",
    mejor_modelo_params$cp_n, mejor_modelo_params$cp_prior, mejor_modelo_params$usar_festivos, mejor_modelo_params$rmse
  ))

  # Gráfica Troubleshooting: Evolución del error (RMSE) en el horizonte de CV
  png(GRAFICA_CV_METRICAS, width = 800, height = 600, res = 100)
  print(plot_cross_validation_metric(df_cv_final, metric = "rmse"))
  dev.off()
} else {
  cat("Validación Cruzada omitida. Usando los primeros valores de las variables globales...\n")
  mejor_modelo_params <- list(
    cp_n = CP_N[1],
    cp_prior = CP_PRIOR_SCALES[1],
    usar_festivos = TRUE # Se asume TRUE por defecto si se salta la CV
  )
}

# ==============================================================================
# PASO 5. AJUSTAR MODELO FINAL Y PREDECIR
# ==============================================================================
# Entrenar el modelo final incorporando la configuración ganadora
modelo_prophet <- prophet(
  n.changepoints = mejor_modelo_params$cp_n,
  changepoint.range = CP_RANGE,
  changepoint.prior.scale = mejor_modelo_params$cp_prior,
  interval.width = INTERVALO_CONFIANZA
)

if (mejor_modelo_params$usar_festivos) {
  modelo_prophet <- add_country_holidays(modelo_prophet, country_name = "MX")
}

modelo_prophet <- fit.prophet(modelo_prophet, TJ_prophet)

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
  select(-(`Ano Nuevo [New Year's Day]`:`Dia del Trabajo [Labour Day] (Observed)_upper`), -(`Natalicio de Benito Juarez [Benito Juarez's birthday]`:`Navidad [Christmas] (Observed)_upper`),
         -starts_with("multiplicative"))
  
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
