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
library(readxl) # Lectura de archivos Excel
library(writexl) # Exportación a formato Excel
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

ARCHIVO_DATOS <- file.path(DIR_DATOS, "Tijuana.xlsx")
SALIDA_ETIQUETA <- "PM2.5_TJ"

# Bandera de Ejecución
REALIZAR_CV <- TRUE # Cambiar a FALSE para omitir Grid Search si ya conoces los parámetros

# Días reservados EXCLUSIVAMENTE para medir el error fuera de muestra (Test Set)
DIAS_PRUEBA <- 90

# Hiperparámetros de Prophet para la Validación Cruzada (Grid Search)
CP_N <- c(25, 30, 42) # Número máximo de puntos de cambio a probar
CP_PRIOR_SCALES <- c(0.01, 0.05, 0.1, 0.005) # Escalas de flexibilidad a probar

# Otros hiperparámetros
CP_RANGE <- 0.8 # Proporción del histórico donde se permiten cambios
INTERVALO_CONFIANZA <- 0.95 # Nivel de confianza para yhat_lower y yhat_upper

# Rutas de salida para los archivos generados
SALIDA_MODELO <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_modelo.Rds"))
SALIDA_HPARAMS <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_hparams.xlsx"))
GRAFICA_CV_METRICAS <- file.path(DIR_RESULTADOS, glue("{SALIDA_ETIQUETA}_metricas.png"))

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

# ==============================================================================
# PASO 4. VALIDACIÓN CRUZADA EN PARALELO (SOLO SOBRE DATOS DE ENTRENAMIENTO)
# ==============================================================================
if (REALIZAR_CV) {
  cat("Iniciando Validación Cruzada en PARALELO para elegir hiperparámetros...\n")

  grid_busqueda <- expand_grid(
    cp_n = CP_N,
    cp_prior = CP_PRIOR_SCALES,
    usar_festivos = c(FALSE, TRUE)
  )

  nucleos_disponibles <- parallelly::availableCores() - 1
  plan(multisession, workers = max(1, nucleos_disponibles))

  cat(sprintf(
    "Evaluando %d combinaciones usando %d núcleos. Por favor espera...\n",
    nrow(grid_busqueda), max(1, nucleos_disponibles)
  ))

  # El Grid Search itera EXCLUSIVAMENTE sobre TJ_train
  resultados_cv <- future_pmap_dfr(grid_busqueda, function(cp_n, cp_prior, usar_festivos) {
    m_temp <- prophet(
      n.changepoints = cp_n,
      changepoint.range = CP_RANGE,
      changepoint.prior.scale = cp_prior
    )

    if (usar_festivos) {
      m_temp <- add_country_holidays(m_temp, country_name = "MX")
    }

    m_temp <- fit.prophet(m_temp, TJ_train)

    df_cv <- cross_validation(m_temp, initial = 365, period = 90, horizon = 60, units = "days")
    df_p <- performance_metrics(df_cv)
    rmse_promedio <- mean(df_p$rmse)

    tibble(
      cp_n = cp_n,
      cp_prior = cp_prior,
      usar_festivos = usar_festivos,
      rmse_cv = rmse_promedio,
      df_cv_data = list(df_cv)
    )
  }, .options = furrr_options(seed = TRUE))

  plan(sequential)

  # Encontrar el modelo ganador
  mejor_resultado <- resultados_cv |>
    arrange(rmse_cv) |>
    slice(1)
  mejor_modelo_params <- as.list(mejor_resultado)
  df_cv_final <- mejor_resultado$df_cv_data[[1]]

  cat(sprintf(
    ">>> Mejores hiperparámetros -> N Puntos: %d | Escala: %f | Festivos: %s\n",
    mejor_modelo_params$cp_n, mejor_modelo_params$cp_prior, mejor_modelo_params$usar_festivos
  ))

  # Gráfica de Métricas (CV interno)
  png(GRAFICA_CV_METRICAS, width = 800, height = 600, res = 100)
  print(plot_cross_validation_metric(df_cv_final, metric = "rmse"))
  dev.off()
} else {
  cat("Validación Cruzada omitida. Usando los primeros valores de las variables globales...\n")
  mejor_modelo_params <- list(
    cp_n = CP_N[1],
    cp_prior = CP_PRIOR_SCALES[1],
    usar_festivos = TRUE
  )
}

# ==============================================================================
# PASO 5. EVALUACIÓN DE ERROR FUERA DE LA MUESTRA (OOS)
# ==============================================================================
cat("\nEvaluando el mejor modelo contra los datos de prueba (Out-of-Sample)...\n")

# Entrenar un modelo temporal solo con datos de entrenamiento usando parámetros ganadores
modelo_eval <- prophet(
  n.changepoints = mejor_modelo_params$cp_n,
  changepoint.range = CP_RANGE,
  changepoint.prior.scale = mejor_modelo_params$cp_prior
)
if (mejor_modelo_params$usar_festivos) {
  modelo_eval <- add_country_holidays(modelo_eval, country_name = "MX")
}

modelo_eval <- fit.prophet(modelo_eval, TJ_train)

# Predecir el periodo de prueba (los últimos DIAS_PRUEBA reservados)
pred_eval_log <- predict(modelo_eval, TJ_test |> select(ds))

# Calcular RMSE fuera de muestra (revirtiendo el logaritmo)
pred_eval <- exp(pred_eval_log$yhat)
real_eval <- exp(TJ_test$y)

rmse_oos <- rmse(real_eval, pred_eval)
mae_oos <- mae(real_eval, pred_eval)

cat(sprintf(">>> Error Real Fuera de Muestra (OOS) -> RMSE: %.2f µg/m³ | MAE: %.2f µg/m³\n", rmse_oos, mae_oos))

# Guardar los hiperparámetros ganadores y sus métricas en Excel
hparams_df <- tibble(
  Parametro = c("N_Changepoints", "Changepoint_Prior_Scale", "Usar_Festivos_MX", "RMSE_Out_of_Sample", "MAE_Out_of_Sample"),
  Valor = c(
    as.character(mejor_modelo_params$cp_n), as.character(mejor_modelo_params$cp_prior),
    as.character(mejor_modelo_params$usar_festivos), as.character(round(rmse_oos, 4)),
    as.character(round(mae_oos, 4))
  )
)
write_xlsx(hparams_df, SALIDA_HPARAMS)

# ==============================================================================
# PASO 6. MODELO DE PRODUCCIÓN (ENTRENAMIENTO AL 100%)
# ==============================================================================
cat("\nEntrenando modelo de producción final con el 100% de los datos...\n")

modelo_produccion <- prophet(
  n.changepoints = mejor_modelo_params$cp_n,
  changepoint.range = CP_RANGE,
  changepoint.prior.scale = mejor_modelo_params$cp_prior,
  interval.width = INTERVALO_CONFIANZA
)

if (mejor_modelo_params$usar_festivos) {
  modelo_produccion <- add_country_holidays(modelo_produccion, country_name = "MX")
}

# Se ajusta con TODA la tabla original limpia (TJ_prophet) para no desperdiciar datos recientes
modelo_produccion <- fit.prophet(modelo_produccion, TJ_prophet)

# ==============================================================================
# PASO 7. GUARDAR MODELO PARA 02-plots.R
# ==============================================================================
saveRDS(modelo_produccion, file = SALIDA_MODELO)
cat(sprintf("\n¡Proceso finalizado con éxito! El modelo listo para predicción se guardó en: %s\n", SALIDA_MODELO))
cat(sprintf("Los hiperparámetros y métricas de error se guardaron en: %s\n", SALIDA_HPARAMS))
