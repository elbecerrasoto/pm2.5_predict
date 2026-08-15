# ==============================================================================
# JUSTFILE: Pipeline de Predicción de PM2.5 con Prophet
# ==============================================================================

# Receta por defecto: muestra la lista de comandos disponibles
default:
    @just --list

# Asegurar permisos de ejecución en el script de R
chmod:
    chmod +x pm2.5_predict.R

# Ejecutar predicción directa en un dataset (sin Cross-Validation)
predict input="datos/Tijuana.tsv" out_dir="resultados" end_date="2026-12-31": chmod
    ./pm2.5_predict.R -i {{input}} -o {{out_dir}} -p {{end_date}}

# Ejecutar predicción CON búsqueda por Validación Cruzada (Grid Search)
cv input="datos/Tijuana.tsv" out_dir="resultados" end_date="2026-12-31": chmod
    ./pm2.5_predict.R -i {{input}} -o {{out_dir}} -c -p {{end_date}}

# Acceso rápido: Ejecución estándar para Tijuana
tijuana:
    just predict input="datos/Tijuana.tsv"

# Acceso rápido: Ejecución con Cross-Validation para Tijuana
tijuana-cv:
    just cv input="datos/Tijuana.tsv"

# Limpiar archivos generados en el directorio de resultados
clean out_dir="resultados":
    rm -rf {{out_dir}}/*.svg {{out_dir}}/*.tsv {{out_dir}}/*.Rds

# Instalar todas las dependencias requeridas en R
install-deps:
    Rscript -e 'install.packages(c("optparse", "tidyverse", "glue", "readxl", "writexl", "prophet", "future", "furrr", "Metrics", "svglite", "parallelly"), repos="https://cloud.r-project.org")'

# Dar formato automático al código R usando el paquete 'styler'
format:
    Rscript -e 'styler::style_file("pm2.5_predict.R")'