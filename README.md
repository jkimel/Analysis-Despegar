# Predicción de Precios Hoteleros en Río de Janeiro 🏖️🏨

![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Markdown](https://img.shields.io/badge/markdown-%23000000.svg?style=for-the-badge&logo=markdown&logoColor=white)
![Status](https://img.shields.io/badge/Status-Completado-success.svg?style=for-the-badge)

## 📌 Descripción del Proyecto

En el competitivo sector del e-commerce turístico, la correcta fijación y estimación de precios (*pricing*) es un pilar fundamental para maximizar la ocupación y el retorno de inversión. Este proyecto aborda el desarrollo de un modelo de **Machine Learning predictivo** capaz de estimar el precio por noche por persona en establecimientos hoteleros de Río de Janeiro.

A partir de un dataset transaccional con características de las búsquedas, atributos del hotel y datos geográficos, el objetivo del proyecto es doble:
1. **Predecir** con alta precisión el valor tarifario.
2. **Interpretar** y cuantificar qué atributos (ej. distancia a la playa, estrellas, valoraciones) tienen el mayor impacto en la disposición a pagar del usuario turístico.

## 🛠️ Stack Tecnológico

El proyecto fue íntegramente desarrollado en **R**, empleando un enfoque moderno y funcional basado en el ecosistema Tidyverse:

- **Procesamiento y Limpieza**: `dplyr`, `tidyr`, `lubridate`, `stringr`
- **Feature Engineering Geográfico**: `geosphere` (cálculo de distancias geoespaciales con la fórmula de Haversine)
- **Modelado Predictivo y Validación**: `rsample` (particionado de datos), `ranger` (implementación altamente optimizada de Random Forest)
- **Visualización Analítica**: `ggplot2` (gráficos formales, manejo avanzado de escalas y temas)

## 🔬 Metodología

El ciclo de vida del proyecto siguió el estándar de la industria para ciencia de datos (CRISP-DM adaptado):

1. **Integración de Datos**: Unión de tres fuentes: historial de búsquedas (`searches`), atributos del hotel con amenities en formato OHE (`datos_hoteles_austral`) y catálogo de comodidades como referencia (`amenities_descriptions`).
2. **Limpieza y Prevención de Data Leakage**: Se eliminaron variables derivadas directamente del precio final y atributos identificadores sin valor predictivo para asegurar la integridad de la evaluación.
3. **Análisis Exploratorio (EDA)**: Análisis de valores faltantes, distribuciones (ej. asimetría en precios), correlaciones bivariadas y comportamiento de compra por posición.
4. **Feature Engineering**: 
   - Extracción de componentes temporales (día de la semana, día del año, feriados brasileños).
   - Cálculo de distancias geoespaciales a puntos de interés turístico (Copacabana, Ipanema, Cristo Redentor) usando la fórmula de Haversine.
   - Features de popularidad del hotel (n° búsquedas, tasa de compra histórica).
   - Dummies de amenities desde columnas OHE reales del dataset de hoteles (parking, spa, pileta, internet, desayuno).
5. **Modelado Baseline**: Implementación de un modelo de Regresión Lineal Simple para establecer un piso de rendimiento base.
6. **Modelado Avanzado**: Entrenamiento de un Ensamble de Árboles (*Random Forest* con 200 estimadores) empleando partición estratificada (80/20 train/test) para capturar no-linealidades complejas.

## 📊 Resultados Clave

El modelo Random Forest superó significativamente la línea base lineal, demostrando la naturaleza altamente no lineal de la fijación de precios hoteleros.

*Métricas sobre el conjunto de prueba (Test Set):*

| Modelo | RMSE | MAE | R² |
|---|---|---|---|
| Baseline lineal (`avgRating`) | $37.51 | $27.88 | 0.1008 (10.1%) |
| **Random Forest (200 árboles)** | **$29.30** | **$21.36** | **0.4513 (45.1%)** |

**Importancia de Variables (Feature Importance — Top 5):**
> *El modelo reveló que los factores temporales y de comportamiento de búsqueda dominan la predicción de precios: la **anticipación de la reserva** (`anticipation`) es el predictor más potente, seguido por el **día del año** (`dia_del_anio`) como proxy de estacionalidad. Entre los atributos del hotel, la **valoración de ubicación** (`avgRatingLocation`) y el **nivel de servicio** (`avgService`, `avgRating`) emergen como los estimadores más relevantes. Esto sugiere que el precio hotelero en Río de Janeiro está fuertemente determinado por cuándo se busca y la calidad percibida del servicio, más que por las comodidades físicas del hotel.*

| Ranking | Variable | Descripción |
|---|---|---|
| 1 | `anticipation` | Días de anticipación de la búsqueda |
| 2 | `dia_del_anio` | Día del año (captura estacionalidad) |
| 3 | `avgRatingLocation` | Rating de ubicación del hotel |
| 4 | `avgService` | Rating de servicio general |
| 5 | `avgRating` | Rating promedio del hotel |

## 📂 Estructura del Repositorio

El proyecto sigue una arquitectura estándar orientada a la reproducibilidad:

```text
├── data/
│   ├── raw/               # Datos originales 
│   └── processed/         # Datos limpios y procesados 
├── scripts/               # Scripts R auxiliares (funciones custom)
├── output/
│   ├── figures/           # Gráficos generados (.pdf)
│   └── reports/           # Documentos finales renderizados (.pdf)
├── .gitignore             # Archivos y directorios excluidos del control de versiones
├── ProyectoHotelero.Rproj # Archivo de proyecto de RStudio
└── README.md              # Documentación principal del proyecto
```

## 🚀 Instrucciones de Ejecución

Para reproducir este análisis en tu entorno local:

1. **Clonar el repositorio**:
   ```bash
   git clone https://github.com/jkimel/hotel-booking-analysis-despegar.git
   ```
2. **Entorno**: Abre el archivo `ProyectoHotelero.Rproj` en RStudio para configurar automáticamente el directorio de trabajo.
3. **Dependencias**: Ejecuta el script `scripts/install_dependencies.R` para asegurar que tienes todas las librerías necesarias.
4. **Ejecución del Código**: Abre y ejecuta el script `scripts/run_model.R`. Este archivo contiene el pipeline completo: desde la carga y limpieza de datos hasta el entrenamiento del modelo Random Forest y la exportación de las figuras a la carpeta `output/figures/`.
