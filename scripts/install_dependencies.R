# ==============================================================================
# Script de Instalación de Dependencias
# Proyecto: Predicción de Precios Hoteleros en Río de Janeiro
# ==============================================================================

paquetes_necesarios <- c(
  "tidyverse",    # Manipulación de datos (dplyr, tidyr, stringr, purrr) y visualización (ggplot2)
  "corrplot",     # Visualización de matrices de correlación
  "lubridate",    # Manejo de fechas y extracción de componentes temporales
  "geosphere",    # Cálculo de distancias geoespaciales (Haversine)
  "rsample",      # Partición estratificada de datos (Train/Test)
  "ranger",       # Implementación optimizada de Random Forest en C++
  "yardstick",    # Métricas de evaluación de modelos (RMSE, MAE, R2)
  "rmarkdown",    # Renderizado del documento
  "knitr"         # Ejecución de chunks de código
)

# Detectar cuáles faltan
paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]

# Instalar los faltantes
if(length(paquetes_faltantes) > 0) {
  cat("Instalando paquetes faltantes:", paste(paquetes_faltantes, collapse = ", "), "\n")
  install.packages(paquetes_faltantes, repos = "https://cloud.r-project.org")
} else {
  cat("Todos los paquetes necesarios ya están instalados en tu entorno.\n")
}
