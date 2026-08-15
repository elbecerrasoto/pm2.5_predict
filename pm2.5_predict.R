#!/usr/bin/env Rscript

# ==============================================================================
# SCRIPT 01: SELECCIÓN DE MODELO Y VALIDACIÓN (01-best_model.R)
# ==============================================================================

# ==============================================================================
# PASO 1. IMPORTAR BIBLIOTECAS NECESARIAS
# ==============================================================================
suppressPackageStartupMessages({
  library(optparse) # Para convertir el script en un CLI
  library(tidyverse) # Manipulación de datos y gráficas (dplyr, ggplot2, lubridate)
  library(glue) # Interpolación de cadenas (strings)
  library(readxl) # Lectura de Excel (por si la entrada es .xlsx)
  library(prophet) # Modelado de series de tiempo
  library(future) # Configuración de procesamiento paralelo
  library(furrr) # Aplicación de funciones en paralelo (purrr + future)
  library(Metrics) # Cálculo rápido de métricas de error (RMSE, MAE)
  library(svglite) # Para exportar svgs correctamente
})

# ==============================================================================
# PASO 2. CONFIGURACIÓN DE INTERFAZ DE LÍNEA DE COMANDOS (CLI)
# ==============================================================================
option_list <- list(
  make_option(c("-i", "--input"),
    type = "character", default = NULL,
    help = "Ruta al archivo de datos de entrada (.tsv, .csv o .xlsx) [REQUERIDO]", metavar = "FILE"
  ),
  make_option(c("-o", "--output_dir"),
    type = "character", default = "resultados",
    help = "Directorio donde se guardarán los resultados [default: %default]"
  ),
  make_option(c("-c", "--cv"),
    action = "store_true", default = FALSE,
    help = "Activa la búsqueda por validación cruzada (si se omite, no se realiza CV)"
  ),
  make_option(c("-p", "--proyeccion_final"),
    type = "character", default = "2026-12-31",
    help = "Fecha final de la proyección en formato YYYY-MM-DD [default: %default]"
  )
)

opt_parser <- OptionParser(option_list = option_list, description = "Ajuste de Prophet y Cross-Validation")
opt <- parse_args(opt_parser)

if (is.null(opt$input)) {
  print_help(opt_parser)
  stop("ERROR CRÍTICO: El parámetro '--input' (-i) es obligatorio.", call. = FALSE)
}

ARCHIVO_DATOS <- opt$input
DIR_RESULTADOS <- opt$output_dir
REALIZAR_CV <- opt$cv
PROYECCION_FINAL <- ymd(opt$proyeccion_final)

if (is.na(PROYECCION_FINAL)) {
  stop("ERROR CRÍTICO: El parámetro '--proyeccion_final' (-p) debe ser una fecha válida en formato YYYY-MM-DD.", call. = FALSE)
}

# ==============================================================================
# PASO 3. VARIABLES GLOBALES Y TEMA
# ==============================================================================
SALIDA_ETIQUETA <- "PM2.5"

NO_CV <- list(
  cp_n = 42,
  cp_prior = 0.1,
  seas_mode = "additive",
  seas_prior = 10,
  usar_festivos = FALSE
)

DIAS_PRUEBA <- 90
CP_N <- c(36, 64)
CP_PRIOR_SCALES <- c(0.1, 0.2)
SEASONALITY_MODES <- c("additive", "multiplicative")
SEASONALITY_PRIOR_SCALES <- c(1.0, 10.0)
CP_RANGE <- 0.8
INTERVALO_CONFIANZA <- 0.95

# Rutas de salida para los archivos generados
SALIDA_MODELO <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_modelo.Rds"))
SALIDA_HPARAMS <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_hparams.tsv"))
SALIDA_TABLA_CV <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_tabla_cv.tsv"))
GRAFICA_CV_METRICAS <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_cvmetricas.svg"))
GRAFICA_OOS_DISPERSION <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_oos_dispersion.svg"))
GRAFICA_OOS_RESIDUALES <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_oos_residuales.svg"))
GRAFICA_OOS_DIST_ERROR <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_oos_dist_error.svg"))

# Gráficas de Producción
GRAFICA_PRINCIPAL <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_prediccion.svg"))
GRAFICA_COMPONENTES <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_componentes.svg"))
GRAFICA_PROD_RESIDUALES <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_prod_residuales.svg"))

tema_profesional <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#F0F0F0", color = NA),
    panel.background = element_rect(fill = "#F0F0F0", color = NA),
    panel.grid.major = element_line(color = "#D9D9D9", linewidth = 0.6),
    panel.grid.minor = element_blank(),
    text = element_text(color = "#333333"),
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

# ==============================================================================
# PASO 4. PREPARACIÓN DE DIRECTORIO, DATOS Y ASERCIONES
# ==============================================================================
if (!dir.exists(DIR_RESULTADOS)) {
  dir.create(DIR_RESULTADOS, recursive = TRUE, showWarnings = FALSE)
}

cat(sprintf("\n[INFO] Cargando datos desde: %s\n", ARCHIVO_DATOS))

if (str_ends(tolower(ARCHIVO_DATOS), "\\.xlsx$")) {
  TJ <- read_excel(ARCHIVO_DATOS)
} else if (str_ends(tolower(ARCHIVO_DATOS), "\\.tsv$|\\.txt$")) {
  TJ <- read_tsv(ARCHIVO_DATOS, show_col_types = FALSE)
} else if (str_ends(tolower(ARCHIVO_DATOS), "\\.csv$")) {
  TJ <- read_csv(ARCHIVO_DATOS, show_col_types = FALSE)
} else {
  stop("ERROR: Formato de archivo no soportado. Debe ser .tsv, .csv o .xlsx")
}

stopifnot("ERROR: Faltan las columnas 'FECHA' y/o 'PM2.5'." = all(c("FECHA", "PM2.5") %in% colnames(TJ)))
stopifnot("ERROR: Se encontraron valores NAs." = !any(is.na(TJ$FECHA)) && !any(is.na(TJ$PM2.5)))
stopifnot("ERROR: Valores <= 0 en PM2.5." = all(TJ$PM2.5 > 0))

TJ_prophet <- TJ |>
  rename(ds = FECHA, y_original = PM2.5) |>
  mutate(ds = as_date(ds), y = log(y_original))

fecha_corte <- max(TJ_prophet$ds) - days(DIAS_PRUEBA)
TJ_train <- TJ_prophet |> filter(ds <= fecha_corte)
TJ_test <- TJ_prophet |> filter(ds > fecha_corte)

cat(sprintf("[INFO] Datos divididos: %d filas de entrenamiento, %d filas para prueba (OOS)\n", nrow(TJ_train), nrow(TJ_test)))

# ==============================================================================
# PASO 5. VALIDACIÓN CRUZADA EN PARALELO
# ==============================================================================
if (REALIZAR_CV) {
  cat("\n[INFO] Iniciando Validación Cruzada en PARALELO para elegir hiperparámetros...\n")

  grid_busqueda <- expand_grid(
    cp_n = CP_N,
    cp_prior = CP_PRIOR_SCALES,
    seas_mode = SEASONALITY_MODES,
    seas_prior = SEASONALITY_PRIOR_SCALES,
    usar_festivos = c(FALSE, TRUE)
  )

  nucleos_disponibles <- parallelly::availableCores() - 1
  plan(multisession, workers = max(1, nucleos_disponibles))

  cat(sprintf("[INFO] Evaluando %d combinaciones usando %d núcleos. Por favor espera...\n\n", nrow(grid_busqueda), max(1, nucleos_disponibles)))

  resultados_cv <- future_pmap_dfr(grid_busqueda, function(cp_n, cp_prior, seas_mode, seas_prior, usar_festivos) {
    m_temp <- prophet(
      n.changepoints = cp_n,
      changepoint.range = CP_RANGE,
      changepoint.prior.scale = cp_prior,
      seasonality.mode = seas_mode,
      seasonality.prior.scale = seas_prior
    )
    if (usar_festivos) m_temp <- add_country_holidays(m_temp, country_name = "MX")

    suppressWarnings(suppressMessages(m_temp <- fit.prophet(m_temp, TJ_train)))

    df_cv <- cross_validation(m_temp, initial = 365, period = 90, horizon = 60, units = "days")
    df_p <- performance_metrics(df_cv)

    tibble(
      cp_n = cp_n, cp_prior = cp_prior, seas_mode = seas_mode, seas_prior = seas_prior,
      usar_festivos = usar_festivos, rmse_cv = mean(df_p$rmse), df_cv_data = list(df_cv)
    )
  }, .options = furrr_options(seed = TRUE), .progress = TRUE)

  plan(sequential)

  resultados_cv_export <- resultados_cv |> select(-df_cv_data)
  write_tsv(resultados_cv_export, SALIDA_TABLA_CV)

  mejor_resultado <- resultados_cv |>
    arrange(rmse_cv) |>
    slice(1)
  mejor_modelo_params <- as.list(mejor_resultado)
  df_cv_final <- mejor_resultado$df_cv_data[[1]]

  cat(sprintf(
    "\n>>> Mejores hiperparámetros -> N Puntos: %d | Escala CP: %f | Modo Seas: %s | Escala Seas: %f | Festivos: %s\n",
    mejor_modelo_params$cp_n, mejor_modelo_params$cp_prior, mejor_modelo_params$seas_mode,
    mejor_modelo_params$seas_prior, mejor_modelo_params$usar_festivos
  ))

  subtitulo_cv <- glue("Modelo Ganador: Estacionalidad {mejor_modelo_params$seas_mode} | Festivos MX: {ifelse(mejor_modelo_params$usar_festivos, 'Sí', 'No')}")

  plot_cv <- suppressWarnings(plot_cross_validation_metric(df_cv_final, metric = "rmse")) +
    tema_profesional +
    labs(
      title = "Degradación del Error (RMSE) en Validación Cruzada",
      subtitle = subtitulo_cv,
      x = "Horizonte de Predicción (Días)", y = "RMSE"
    )

  ggsave(GRAFICA_CV_METRICAS, plot = plot_cv, width = 9, height = 6)
} else {
  cat("\n[INFO] Validación Cruzada omitida (bandera --cv no presente). Usando valores configurados por defecto...\n")
  mejor_modelo_params <- NO_CV
}

# ==============================================================================
# PASO 6. EVALUACIÓN DE ERROR FUERA DE LA MUESTRA (OOS)
# ==============================================================================
cat("\n[INFO] Evaluando el mejor modelo contra los datos de prueba (Out-of-Sample)...\n")

modelo_eval <- prophet(
  n.changepoints = mejor_modelo_params$cp_n,
  changepoint.range = CP_RANGE,
  changepoint.prior.scale = mejor_modelo_params$cp_prior,
  seasonality.mode = mejor_modelo_params$seas_mode,
  seasonality.prior.scale = mejor_modelo_params$seas_prior
)
if (mejor_modelo_params$usar_festivos) modelo_eval <- add_country_holidays(modelo_eval, country_name = "MX")
suppressWarnings(suppressMessages(modelo_eval <- fit.prophet(modelo_eval, TJ_train)))

pred_eval_log <- predict(modelo_eval, TJ_test |> select(ds))
pred_eval <- exp(pred_eval_log$yhat)
real_eval <- exp(TJ_test$y)

rmse_oos <- rmse(real_eval, pred_eval)
mae_oos <- mae(real_eval, pred_eval)

cat(sprintf(">>> Error Real Fuera de Muestra (OOS) -> RMSE: %.2f µg/m³ | MAE: %.2f µg/m³\n", rmse_oos, mae_oos))

df_oos <- tibble(Fecha = TJ_test$ds, Real = real_eval, Prediccion = pred_eval, Error = Real - Prediccion)

p_dispersion <- ggplot(df_oos, aes(x = Real, y = Prediccion)) +
  geom_point(alpha = 0.6, color = "#2C3E50") +
  geom_abline(intercept = 0, slope = 1, color = "#E74C3C", linetype = "dashed", linewidth = 1) +
  labs(title = "Predicción vs. Valores Reales (OOS)", subtitle = "La línea roja indica una predicción perfecta (x=y)", x = "Concentración Real de PM2.5", y = "Predicción del Modelo") +
  tema_profesional
ggsave(GRAFICA_OOS_DISPERSION, plot = p_dispersion, width = 8, height = 6)

p_residuales <- ggplot(df_oos, aes(x = Fecha, y = Error)) +
  geom_segment(aes(xend = Fecha, yend = 0), color = "#95A5A6", alpha = 0.5) +
  geom_point(color = "#2980B9", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#E74C3C", linetype = "dashed") +
  labs(title = "Evolución Temporal de los Errores (OOS)", subtitle = "Verificación de patrones no capturados (Heterocedasticidad)", x = "Fecha", y = "Error (Real - Predicción)") +
  tema_profesional
ggsave(GRAFICA_OOS_RESIDUALES, plot = p_residuales, width = 10, height = 5)

p_distribucion <- ggplot(df_oos, aes(x = Error)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "#BDC3C7", color = "white") +
  geom_density(color = "#2C3E50", linewidth = 1) +
  geom_vline(xintercept = 0, color = "#E74C3C", linetype = "dashed") +
  labs(title = "Distribución de los Errores de Predicción", subtitle = "Una curva centrada en 0 indica que el modelo no está sesgado", x = "Error de Predicción", y = "Densidad") +
  tema_profesional
ggsave(GRAFICA_OOS_DIST_ERROR, plot = p_distribucion, width = 8, height = 6)

# Exportar hiperparámetros (incluyendo la ruta del archivo, la fecha de ejecución y parámetros globales)
hparams_df <- tibble(
  Parametro = c(
    "Timestamp_Ejecucion",
    "Archivo_Datos",
    "Dias_Prueba_OOS",
    "Proyeccion_Final",
    "N_Changepoints",
    "Changepoint_Prior_Scale",
    "Seasonality_Mode",
    "Seasonality_Prior_Scale",
    "Usar_Festivos_MX",
    "RMSE_Out_of_Sample",
    "MAE_Out_of_Sample"
  ),
  Valor = c(
    as.character(Sys.time()),
    ARCHIVO_DATOS,
    as.character(DIAS_PRUEBA),
    as.character(PROYECCION_FINAL),
    as.character(mejor_modelo_params$cp_n),
    as.character(mejor_modelo_params$cp_prior),
    mejor_modelo_params$seas_mode,
    as.character(mejor_modelo_params$seas_prior),
    as.character(mejor_modelo_params$usar_festivos),
    as.character(round(rmse_oos, 4)),
    as.character(round(mae_oos, 4))
  )
)
write_tsv(hparams_df, SALIDA_HPARAMS)

# ==============================================================================
# PASO 7. MODELO DE PRODUCCIÓN (ENTRENAMIENTO AL 100%) Y GRÁFICAS FINALES
# ==============================================================================
cat("\n[INFO] Entrenando modelo de producción final con el 100% de los datos...\n")

modelo_produccion <- prophet(
  n.changepoints = mejor_modelo_params$cp_n,
  changepoint.range = CP_RANGE,
  changepoint.prior.scale = mejor_modelo_params$cp_prior,
  seasonality.mode = mejor_modelo_params$seas_mode,
  seasonality.prior.scale = mejor_modelo_params$seas_prior,
  interval.width = INTERVALO_CONFIANZA
)
if (mejor_modelo_params$usar_festivos) modelo_produccion <- add_country_holidays(modelo_produccion, country_name = "MX")
suppressWarnings(suppressMessages(modelo_produccion <- fit.prophet(modelo_produccion, TJ_prophet)))

# ---- 7.1 CALCULAR PRONÓSTICO (IN-SAMPLE + OUT-OF-SAMPLE) ----
fecha_ultima <- max(TJ_prophet$ds)
PROYECCION_INICIO <- fecha_ultima + days(1)
dias_futuros <- as.numeric(PROYECCION_FINAL - fecha_ultima)

futuro_prod <- make_future_dataframe(modelo_produccion, periods = dias_futuros, freq = "day")
resultados_prod_log <- predict(modelo_produccion, futuro_prod)

resultados_prod <- resultados_prod_log |>
  mutate(ds = as_date(ds), yhat = exp(yhat), yhat_lower = exp(yhat_lower), yhat_upper = exp(yhat_upper)) |>
  filter(ds <= PROYECCION_FINAL)

# ---- 7.2 RESIDUALES DEL MODELO DE PRODUCCIÓN (IN-SAMPLE) ----
df_prod_eval <- resultados_prod |>
  filter(ds <= fecha_ultima) |>
  select(ds, yhat) |>
  inner_join(TJ_prophet |> select(ds, y_original), by = "ds") |>
  mutate(Error = y_original - yhat)

p_residuales_prod <- ggplot(df_prod_eval, aes(x = ds, y = Error)) +
  geom_segment(aes(xend = ds, yend = 0), color = "#95A5A6", alpha = 0.5) +
  geom_point(color = "#16A085", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "#E74C3C", linetype = "dashed") +
  labs(
    title = "Residuales Intramuestra (Modelo de Producción)",
    subtitle = "Errores del ajuste final entrenado con el 100% de la historia",
    x = "Fecha", y = "Error (Real - Predicción)"
  ) +
  tema_profesional

ggsave(GRAFICA_PROD_RESIDUALES, plot = p_residuales_prod, width = 10, height = 5)

# ---- 7.3 GRÁFICO PRINCIPAL DE PRONÓSTICO CON LEYENDAS (PAPER-READY) ----
grafica_principal <- ggplot() +
  geom_ribbon(data = resultados_prod, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "#B0BEC5", alpha = 0.4) +
  geom_vline(xintercept = PROYECCION_INICIO, color = "#2C3E50", linetype = "dashed", linewidth = 1) +
  geom_point(data = TJ_prophet, aes(x = ds, y = y_original, color = "Valor Real"), size = 1.2, alpha = 0.5) +
  geom_line(data = resultados_prod, aes(x = ds, y = yhat, color = "Valor Proyectado"), linewidth = 1) +
  scale_color_manual(name = "", values = c("Valor Real" = "#5D6D7E", "Valor Proyectado" = "#D81B60")) +
  tema_profesional +
  labs(
    title = "Pronóstico de PM2.5",
    subtitle = paste("Predicción proyectada hasta el", format(PROYECCION_FINAL, "%d de %B, %Y")),
    x = "Fecha", y = "Concentración de PM2.5 (µg/m³)"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months")

ggsave(GRAFICA_PRINCIPAL, plot = grafica_principal, width = 12, height = 7)

# ---- 7.4 GRÁFICA DE COMPONENTES ----
svglite::svglite(GRAFICA_COMPONENTES, width = 10, height = 8)
suppressWarnings(prophet_plot_components(modelo_produccion, resultados_prod_log))
invisible(dev.off())

# ==============================================================================
# PASO 8. GUARDAR MODELO PARA 02-plots.R
# ==============================================================================
saveRDS(modelo_produccion, file = SALIDA_MODELO)
cat(sprintf("\n[INFO] ¡Proceso finalizado! El modelo se guardó en: %s\n", SALIDA_MODELO))
