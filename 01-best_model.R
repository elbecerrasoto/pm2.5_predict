# ==============================================================================
# SCRIPT 01: SELECCIÓN DE MODELO Y VALIDACIÓN (01-best_model.R)
# ==============================================================================
# Hace cross validation para encontrar el mejor modelo.
# Además incluye metodología de "Hold-out test" (Entrenamiento/Prueba) para
# estimar el error real fuera de la muestra una vez encontrado el mejor modelo.

# ==============================================================================
# PASO 1. IMPORTAR BIBLIOTECAS NECESARIAS
# ==============================================================================
library(tidyverse) # Manipulación de datos y gráficas (dplyr, ggplot2, lubridate)
library(glue) # Interpolación de cadenas (strings)
library(prophet) # Modelado de series de tiempo
library(future) # Configuración de procesamiento paralelo
library(furrr) # Aplicación de funciones en paralelo (purrr + future)
library(Metrics) # Cálculo rápido de métricas de error (RMSE, MAE)

# ==============================================================================
# PASO 2. VARIABLES GLOBALES Y DE CONFIGURACIÓN
# ==============================================================================
# Archivos y Directorios
DIR_RESULTADOS <- "resultados"
DIR_DATOS <- "datos"

ARCHIVO_DATOS <- file.path(DIR_DATOS, "Tijuana.tsv")
SALIDA_ETIQUETA <- "PM2.5_TJ"

# Bandera de Ejecución
REALIZAR_CV <- FALSE
# Valores a usar cuando REALIZAR_CV es FALSE
NO_CV <- list(
  cp_n = 42,
  cp_prior = 0.1,
  seas_mode = "additive",
  seas_prior = 10,
  usar_festivos = FALSE
)

# Días reservados EXCLUSIVAMENTE para medir el error fuera de muestra (Test Set)
DIAS_PRUEBA <- 90

# Hiperparámetros de Prophet para la Validación Cruzada (Grid Search Ampliado)
CP_N <- c(36, 42, 64) #
CP_PRIOR_SCALES <- c(0.05, 0.1, 0.2, 0.5)
SEASONALITY_MODES <- c("additive", "multiplicative") # Aditivo vs Multiplicativo
SEASONALITY_PRIOR_SCALES <- c(0.1, 1.0, 10.0) # Rigidez de la estacionalidad

# Otros hiperparámetros
CP_RANGE <- 0.8
INTERVALO_CONFIANZA <- 0.95

# Rutas de salida para los archivos generados (Actualizados a SVG)
SALIDA_MODELO <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_modelo.Rds"))
SALIDA_HPARAMS <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_hparams.tsv"))
GRAFICA_CV_METRICAS <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_cvmetricas.svg"))
GRAFICA_OOS_DISPERSION <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_oos_dispersion.svg"))
GRAFICA_OOS_RESIDUALES <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_oos_residuales.svg"))
GRAFICA_OOS_DIST_ERROR <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_oos_dist_error.svg"))

# ==============================================================================
# PASO 3. PREPARACIÓN DE DIRECTORIO, DATOS Y ASERCIONES
# ==============================================================================
if (!dir.exists(DIR_RESULTADOS)) {
  dir.create(DIR_RESULTADOS, recursive = TRUE, showWarnings = FALSE)
}

TJ <- read_tsv(ARCHIVO_DATOS)

stopifnot("ERROR: Faltan las columnas 'FECHA' y/o 'PM2.5'." = all(c("FECHA", "PM2.5") %in% colnames(TJ)))
stopifnot("ERROR: Se encontraron valores NAs." = !any(is.na(TJ$FECHA)) && !any(is.na(TJ$PM2.5)))
stopifnot("ERROR: Valores <= 0 en PM2.5." = all(TJ$PM2.5 > 0))

TJ_prophet <- TJ |>
  rename(ds = FECHA, y_original = PM2.5) |>
  mutate(ds = as_date(ds), y = log(y_original))

fecha_corte <- max(TJ_prophet$ds) - days(DIAS_PRUEBA)
TJ_train <- TJ_prophet |> filter(ds <= fecha_corte)
TJ_test <- TJ_prophet |> filter(ds > fecha_corte)

cat(sprintf("Datos divididos: %d filas de entrenamiento, %d filas para prueba (OOS)\n", nrow(TJ_train), nrow(TJ_test)))

# ==============================================================================
# PASO 4. VALIDACIÓN CRUZADA EN PARALELO
# ==============================================================================
if (REALIZAR_CV) {
  cat("Iniciando Validación Cruzada en PARALELO para elegir hiperparámetros...\n")

  grid_busqueda <- expand_grid(
    cp_n = CP_N,
    cp_prior = CP_PRIOR_SCALES,
    seas_mode = SEASONALITY_MODES,
    seas_prior = SEASONALITY_PRIOR_SCALES,
    usar_festivos = c(FALSE, TRUE)
  )

  nucleos_disponibles <- parallelly::availableCores() - 1
  plan(multisession, workers = max(1, nucleos_disponibles))

  cat(sprintf("Evaluando %d combinaciones usando %d núcleos. Por favor espera...\n", nrow(grid_busqueda), max(1, nucleos_disponibles)))

  resultados_cv <- future_pmap_dfr(grid_busqueda, function(cp_n, cp_prior, seas_mode, seas_prior, usar_festivos) {
    m_temp <- prophet(
      n.changepoints = cp_n,
      changepoint.range = CP_RANGE,
      changepoint.prior.scale = cp_prior,
      seasonality.mode = seas_mode,
      seasonality.prior.scale = seas_prior
    )
    if (usar_festivos) m_temp <- add_country_holidays(m_temp, country_name = "MX")
    m_temp <- fit.prophet(m_temp, TJ_train)

    df_cv <- cross_validation(m_temp, initial = 365, period = 90, horizon = 60, units = "days")
    df_p <- performance_metrics(df_cv)

    tibble(
      cp_n = cp_n, cp_prior = cp_prior, seas_mode = seas_mode, seas_prior = seas_prior,
      usar_festivos = usar_festivos, rmse_cv = mean(df_p$rmse), df_cv_data = list(df_cv)
    )
  }, .options = furrr_options(seed = TRUE))

  plan(sequential)

  mejor_resultado <- resultados_cv |>
    arrange(rmse_cv) |>
    slice(1)
  mejor_modelo_params <- as.list(mejor_resultado)
  df_cv_final <- mejor_resultado$df_cv_data[[1]]

  cat(sprintf(
    ">>> Mejores hiperparámetros -> N Puntos: %d | Escala CP: %f | Modo Seas: %s | Escala Seas: %f | Festivos: %s\n",
    mejor_modelo_params$cp_n, mejor_modelo_params$cp_prior, mejor_modelo_params$seas_mode,
    mejor_modelo_params$seas_prior, mejor_modelo_params$usar_festivos
  ))

  # gráfica de métricas CV
  plot_cv <- plot_cross_validation_metric(df_cv_final, metric = "rmse") +
    labs(
      title = "Degradación del Error (RMSE) en Validación Cruzada",
      subtitle = "Comportamiento del error conforme aumenta el horizonte de predicción",
      x = "Horizonte de Predicción (Días)", y = "RMSE"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

  ggsave(GRAFICA_CV_METRICAS, plot = plot_cv, width = 9, height = 6)
} else {
  cat("Validación Cruzada omitida. Usando los primeros valores...\n")
  mejor_modelo_params <- NO_CV
}

# ==============================================================================
# PASO 5. EVALUACIÓN DE ERROR FUERA DE LA MUESTRA (OOS)
# ==============================================================================
cat("\nEvaluando el mejor modelo contra los datos de prueba (Out-of-Sample)...\n")

modelo_eval <- prophet(
  n.changepoints = mejor_modelo_params$cp_n,
  changepoint.range = CP_RANGE,
  changepoint.prior.scale = mejor_modelo_params$cp_prior,
  seasonality.mode = mejor_modelo_params$seas_mode,
  seasonality.prior.scale = mejor_modelo_params$seas_prior
)
if (mejor_modelo_params$usar_festivos) modelo_eval <- add_country_holidays(modelo_eval, country_name = "MX")
modelo_eval <- fit.prophet(modelo_eval, TJ_train)

pred_eval_log <- predict(modelo_eval, TJ_test |> select(ds))
pred_eval <- exp(pred_eval_log$yhat)
real_eval <- exp(TJ_test$y)

rmse_oos <- rmse(real_eval, pred_eval)
mae_oos <- mae(real_eval, pred_eval)

cat(sprintf(">>> Error Real Fuera de Muestra (OOS) -> RMSE: %.2f µg/m³ | MAE: %.2f µg/m³\n", rmse_oos, mae_oos))

# ---- GENERACIÓN DE GRÁFICOS DE SALUD DEL MODELO (OOS) ----
df_oos <- tibble(Fecha = TJ_test$ds, Real = real_eval, Prediccion = pred_eval, Error = Real - Prediccion)

# 1. Gráfica de Dispersión (Real vs Predicción)
p_dispersion <- ggplot(df_oos, aes(x = Real, y = Prediccion)) +
  geom_point(alpha = 0.6, color = "#2C3E50") +
  geom_abline(intercept = 0, slope = 1, color = "#E74C3C", linetype = "dashed", size = 1) +
  labs(title = "Predicción vs. Valores Reales (OOS)", subtitle = "La línea roja indica una predicción perfecta (x=y)", x = "Concentración Real de PM2.5", y = "Predicción del Modelo") +
  theme_minimal()
ggsave(GRAFICA_OOS_DISPERSION, plot = p_dispersion, width = 8, height = 6)

# 2. Gráfica de Residuales en el Tiempo
p_residuales <- ggplot(df_oos, aes(x = Fecha, y = Error)) +
  geom_segment(aes(xend = Fecha, yend = 0), color = "#95A5A6", alpha = 0.5) +
  geom_point(color = "#2980B9", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#E74C3C", linetype = "dashed") +
  labs(title = "Evolución Temporal de los Errores (OOS)", subtitle = "Verificación de patrones no capturados (Heterocedasticidad)", x = "Fecha", y = "Error (Real - Predicción)") +
  theme_minimal()
ggsave(GRAFICA_OOS_RESIDUALES, plot = p_residuales, width = 10, height = 5)

# 3. Distribución de los Errores (Campana)
p_distribucion <- ggplot(df_oos, aes(x = Error)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "#BDC3C7", color = "white") +
  geom_density(color = "#2C3E50", size = 1) +
  geom_vline(xintercept = 0, color = "#E74C3C", linetype = "dashed") +
  labs(title = "Distribución de los Errores de Predicción", subtitle = "Una curva centrada en 0 indica que el modelo no está sesgado", x = "Error de Predicción", y = "Densidad") +
  theme_minimal()
ggsave(GRAFICA_OOS_DIST_ERROR, plot = p_distribucion, width = 8, height = 6)

# Exportar hiperparámetros
hparams_df <- tibble(
  Parametro = c("N_Changepoints", "Changepoint_Prior_Scale", "Seasonality_Mode", "Seasonality_Prior_Scale", "Usar_Festivos_MX", "RMSE_Out_of_Sample", "MAE_Out_of_Sample"),
  Valor = c(as.character(mejor_modelo_params$cp_n), as.character(mejor_modelo_params$cp_prior), mejor_modelo_params$seas_mode, as.character(mejor_modelo_params$seas_prior), as.character(mejor_modelo_params$usar_festivos), as.character(round(rmse_oos, 4)), as.character(round(mae_oos, 4)))
)
write_tsv(hparams_df, SALIDA_HPARAMS)

# ==============================================================================
# PASO 6. MODELO DE PRODUCCIÓN (ENTRENAMIENTO AL 100%)
# ==============================================================================
cat("\nEntrenando modelo de producción final con el 100% de los datos...\n")

modelo_produccion <- prophet(
  n.changepoints = mejor_modelo_params$cp_n,
  changepoint.range = CP_RANGE,
  changepoint.prior.scale = mejor_modelo_params$cp_prior,
  seasonality.mode = mejor_modelo_params$seas_mode,
  seasonality.prior.scale = mejor_modelo_params$seas_prior,
  interval.width = INTERVALO_CONFIANZA
)
if (mejor_modelo_params$usar_festivos) modelo_produccion <- add_country_holidays(modelo_produccion, country_name = "MX")
modelo_produccion <- fit.prophet(modelo_produccion, TJ_prophet)

# ==============================================================================
# PASO 7. GUARDAR MODELO PARA 02-plots.R
# ==============================================================================
saveRDS(modelo_produccion, file = SALIDA_MODELO)
cat(sprintf("\n¡Proceso finalizado! El modelo se guardó en: %s\n", SALIDA_MODELO))
