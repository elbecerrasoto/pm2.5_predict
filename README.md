# Pronóstico de Calidad del Aire ($PM_{2.5}$) con Prophet

Este proyecto proporciona un pipeline automatizado en **R** para el modelado, validación cruzada, evaluación fuera de muestra (*Out-of-Sample*) y generación de proyecciones de calidad del aire ($PM_{2.5}$) utilizando **Facebook Prophet**.

Está diseñado bajo estándares rigurosos de investigación científica y reproducibilidad:
* **Log-Transformación pura ($\log(y)$):** Evita predicciones de concentraciones negativas sin distorsionar los datos con constantes arbitrarias.
* **Validación Cruzada en Paralelo:** Búsqueda en cuadrícula (*Grid Search*) acelerada mediante procesamiento multinúcleo con barra de progreso.
* **Evaluación Fuera de Muestra (OOS):** División estricta en conjunto de datos de entrenamiento y prueba (*Hold-out set*) para estimar el error real ($RMSE$ y $MAE$).
* **Visualizaciones Vectoriales (`.svg`):** Gráficas con temas personalizados (inspirados en *FiveThirtyEight*), listas para su inclusión en publicaciones científicas y presentaciones.

---

## 📁 Estructura del Proyecto

```text
.
├── pm2.5_predict.R        # Script principal ejecutable (R CLI)
├── justfile               # Automatización de tareas (Just task runner)
├── datos/
│   └── Tijuana.tsv        # Datos de entrada (ejemplo)
└── resultados/            # Archivos y gráficas generadas automáticamente
    ├── PM2.5_modelo.Rds
    ├── PM2.5_hparams.tsv
    ├── PM2.5_tabla_cv.tsv
    ├── PM2.5_prediccion.svg
    ├── PM2.5_componentes.svg
    ├── PM2.5_prod_residuales.svg
    ├── PM2.5_cvmetricas.svg
    ├── PM2.5_oos_dispersion.svg
    ├── PM2.5_oos_residuales.svg
    └── PM2.5_oos_dist_error.svg