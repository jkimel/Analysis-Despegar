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

1. **Limpieza y Prevención de Data Leakage**: Se eliminaron variables derivadas directamente del precio final y atributos identificadores sin valor predictivo para asegurar la integridad de la evaluación.
2. **Análisis Exploratorio (EDA)**: Análisis de valores faltantes, distribuciones (ej. asimetría en precios), y correlaciones bivariadas.
3. **Feature Engineering**: 
   - Extracción de componentes temporales (día de la semana, feriados).
   - Cálculo de distancias a puntos de interés turístico (Copacabana, Ipanema, Cristo Redentor).
4. **Modelado Baseline**: Implementación de un modelo de Regresión Lineal Simple para establecer un piso de rendimiento base.
5. **Modelado Avanzado**: Entrenamiento de un Ensamble de Árboles (*Random Forest* con 200 estimadores) empleando partición estratificada (80/20 train/test) para capturar no-linealidades complejas.

## 📊 Resultados Clave

El modelo Random Forest superó significativamente la línea base lineal, demostrando la naturaleza altamente no lineal de la fijación de precios hoteleros.

*Métricas sobre el conjunto de prueba (Test Set):*
- **RMSE (Root Mean Squared Error):** $ 27.81
- **MAE (Mean Absolute Error):** $ 20.11
- **R² (Varianza explicada):** 0.5059 (50.6%)

**Importancia de Variables (Feature Importance):**
> *A través de la reducción de impureza, el modelo reveló de forma unívoca que las clasificaciones de estatus del hotel (`starRating` y `avgRating`) junto con la ubicación geográfica (distancia a Copacabana) son los estimadores principales del precio. En consecuencia, el excedente del consumidor turístico se canaliza fuertemente hacia atributos posicionales y jerárquicos, relegando las comodidades accesorias (amenities) a un impacto meramente marginal.*

## 📂 Estructura del Repositorio

El proyecto sigue una arquitectura estándar orientada a la reproducibilidad:

```text
├── data/
│   ├── raw/               # Datos originales 
│   └── processed/         # Datos limpios y procesados 
├── scripts/               # Scripts R auxiliares (funciones custom)
├── notebooks/             # Archivos Markdown con el análisis completo
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
4. **Ejecución**: Abre el archivo `notebooks/prediccion_precios.Rmd` y ejecuta los bloques secuencialmente, o utiliza el comando `Knit` (Render) para generar el reporte completo.
