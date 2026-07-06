## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)


## ----directorio, eval=FALSE---------------------------------------------------
# # Fija el directorio al ejecutar chunks de forma interactiva en RStudio
# # NOTA: eval=FALSE — este chunk solo se ejecuta manualmente en RStudio.
# #       Al usar rmarkdown::render() desde consola no es necesario porque
# #       el directorio de trabajo se fija antes de llamar a render().
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()  # Verificamos que sea correcto


## ----librerias----------------------------------------------------------------
library(tidyverse)
library(corrplot)
library(lubridate)
if (!requireNamespace("geosphere", quietly = TRUE)) {
  install.packages("geosphere", repos = "https://cloud.r-project.org")
}
library(geosphere)


## ----carga_searches-----------------------------------------------------------
searches <- read.csv("../data/raw/searches_6381.csv", stringsAsFactors = FALSE)


## ----carga_amenities----------------------------------------------------------
amenities <- read.csv("../data/raw/amenities_descriptions.csv", stringsAsFactors = FALSE)


## ----dim_searches-------------------------------------------------------------
cat("Filas:   ", nrow(searches), "\n")
cat("Columnas:", ncol(searches), "\n")


## ----cols_searches------------------------------------------------------------
cat("Columnas del dataset Searches:\n")
print(colnames(searches))


## ----head_searches------------------------------------------------------------
head(searches)


## ----dim_amenities------------------------------------------------------------
cat("Filas:   ", nrow(amenities), "\n")
cat("Columnas:", ncol(amenities), "\n")


## ----cols_amenities-----------------------------------------------------------
cat("Columnas del dataset Amenities:\n")
print(colnames(amenities))


## ----head_amenities-----------------------------------------------------------
head(amenities)


## ----union_datasets-----------------------------------------------------------
# Convertimos amen_ohe_extraflag a character para que los tipos sean compatibles
searches_chr <- searches %>%
  mutate(amen_ohe_extraflag = as.character(amen_ohe_extraflag))

amenities_join <- amenities %>%
  rename(amen_ohe_extraflag = id)   # id ya es character

# Left join: conserva TODAS las filas de searches
dataset_unido <- searches_chr %>%
  left_join(amenities_join, by = "amen_ohe_extraflag")

cat("Filas del dataset unido:   ", nrow(dataset_unido), "\n")
cat("Columnas del dataset unido:", ncol(dataset_unido), "\n")

# Cuantas filas tuvieron match con amenities?
matches <- sum(!is.na(dataset_unido$descriptions.es))
cat("Filas con amenity asociada:", matches, "\n")
cat("Filas sin match (NA):      ", nrow(dataset_unido) - matches, "\n")


## ----cols_unido---------------------------------------------------------------
cat("Columnas del dataset unido:\n")
print(colnames(dataset_unido))


## ----head_unido---------------------------------------------------------------
head(dataset_unido)


## ----eda_estructura-----------------------------------------------------------
glimpse(dataset_unido)


## ----eda_summary--------------------------------------------------------------
summary(dataset_unido)


## ----eda_nas------------------------------------------------------------------
# Conteo y porcentaje de NA por columna
na_df <- dataset_unido %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(pct_na = round(na_count / nrow(dataset_unido) * 100, 2)) %>%
  filter(na_count > 0) %>%
  arrange(desc(pct_na))

print(na_df)


## ----eda_nas_plot, fig.width=9, fig.height=5----------------------------------
if (nrow(na_df) > 0) {
  ggplot(na_df, aes(x = reorder(variable, pct_na), y = pct_na)) +
    geom_col(fill = "#E63946", alpha = 0.85) +
    geom_text(aes(label = paste0(pct_na, "%")), hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(
      title = "Porcentaje de valores faltantes por variable",
      x = "Variable", y = "% de NA"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
} else {
  cat("No hay valores faltantes en el dataset.\n")
}


## ----eda_target_tabla---------------------------------------------------------
target_resumen <- dataset_unido %>%
  count(target) %>%
  mutate(
    etiqueta  = ifelse(target == 1, "Compro (1)", "No compro (0)"),
    porcentaje = round(n / sum(n) * 100, 2)
  )
print(target_resumen)


## ----eda_target_plot, fig.width=6, fig.height=4-------------------------------
ggplot(target_resumen, aes(x = etiqueta, y = porcentaje, fill = etiqueta)) +
  geom_col(width = 0.5, alpha = 0.9) +
  geom_text(aes(label = paste0(porcentaje, "%")), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Compro (1)" = "#2DC653", "No compro (0)" = "#E63946")) +
  labs(
    title = "Distribucion de la variable objetivo (target)",
    x = NULL, y = "Porcentaje (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))


## ----eda_precios_stats--------------------------------------------------------
dataset_unido %>%
  select(price_by_night, price_by_night_adult, price_by_night_person, min_query_price) %>%
  summary()


## ----eda_precio_hist, fig.width=9, fig.height=4-------------------------------
ggplot(dataset_unido %>% filter(price_by_night > 0), aes(x = price_by_night)) +
  geom_histogram(bins = 60, fill = "#457B9D", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(price_by_night, na.rm = TRUE)),
             color = "#E63946", linetype = "dashed", linewidth = 1) +
  annotate("text", x = median(dataset_unido$price_by_night, na.rm = TRUE),
           y = Inf, label = paste0("Mediana: $", round(median(dataset_unido$price_by_night, na.rm = TRUE), 1)),
           vjust = 2, hjust = -0.1, color = "#E63946", fontface = "bold") +
  scale_x_log10(labels = scales::dollar_format()) +
  labs(
    title = "Distribucion del precio por noche (escala logaritmica)",
    subtitle = "Eje X en escala log10 para mejor visualizacion de la distribucion sesgada",
    x = "Precio por noche (USD) — log10", y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


## ----eda_precio_target, fig.width=9, fig.height=4-----------------------------
ggplot(dataset_unido %>% filter(price_by_night > 0),
       aes(x = factor(target), y = price_by_night, fill = factor(target))) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
  scale_fill_manual(values = c("0" = "#E63946", "1" = "#2DC653"),
                    labels = c("No compro", "Compro")) +
  scale_y_log10(labels = scales::dollar_format()) +
  labs(
    title = "Precio por noche segun si el usuario compro o no (escala logaritmica)",
    subtitle = "Eje Y en escala log10",
    x = "Target (0 = No compro | 1 = Compro)", y = "Precio por noche (USD) — log10", fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


## ----eda_estrellas, fig.width=7, fig.height=4---------------------------------
dataset_unido %>%
  count(starRating) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = factor(starRating), y = pct, fill = factor(starRating))) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, fontface = "bold") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(
    title = "Distribucion de hoteles por categoria de estrellas",
    x = "Estrellas", y = "Porcentaje (%)", fill = "Estrellas"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


## ----eda_ratings_stats--------------------------------------------------------
dataset_unido %>%
  select(avgRating, avgRatingCleaning, avgRatingInternetAccessAndQuality,
         avgRatingLocation, avgQualityprice, avgServicepersonal, avgService) %>%
  summary()


## ----eda_ratings_plot, fig.width=10, fig.height=5-----------------------------
dataset_unido %>%
  select(avgRating, avgRatingCleaning, avgRatingInternetAccessAndQuality,
         avgRatingLocation, avgQualityprice, avgServicepersonal, avgService) %>%
  pivot_longer(everything(), names_to = "rating_tipo", values_to = "valor") %>%
  ggplot(aes(x = reorder(rating_tipo, valor, FUN = median), y = valor, fill = rating_tipo)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.15) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribucion de ratings por dimension",
    x = NULL, y = "Puntaje (0-100)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))


## ----eda_paises, fig.width=8, fig.height=4------------------------------------
paises <- c("AR", "BR", "CL", "CO", "MX", "PE", "UY", "OTHER")
dataset_unido %>%
  select(all_of(paises)) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "pais", values_to = "busquedas") %>%
  mutate(pct = round(busquedas / sum(busquedas) * 100, 1)) %>%
  ggplot(aes(x = reorder(pais, busquedas), y = busquedas, fill = pais)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, fontface = "bold") +
  coord_flip() +
  scale_y_log10(labels = scales::comma_format()) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Busquedas por pais de origen del usuario (escala logaritmica)",
    subtitle = "Eje X en escala log10 para comparar paises con grandes diferencias en volumen",
    x = "Pais", y = "Cantidad de busquedas — log10"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))


## ----eda_anticipacion, fig.width=9, fig.height=4------------------------------
ggplot(dataset_unido %>% filter(anticipation > 0), aes(x = anticipation)) +
  geom_histogram(bins = 50, fill = "#6A4C93", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(anticipation, na.rm = TRUE)),
             color = "#F4A261", linetype = "dashed", linewidth = 1) +
  scale_x_log10() +
  labs(
    title = "Distribucion de la anticipacion de reserva (escala logaritmica)",
    subtitle = "Eje X en escala log10 — Dias entre la fecha de busqueda y el check-in",
    x = "Dias de anticipacion — log10", y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


## ----eda_duracion, fig.width=8, fig.height=4----------------------------------
dataset_unido %>%
  count(duration) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  filter(duration <= 20) %>%
  ggplot(aes(x = factor(duration), y = n, fill = factor(duration))) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3) +
  scale_y_log10(labels = scales::comma_format()) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Distribucion de la duracion de la estadia (escala logaritmica)",
    subtitle = "Eje Y en escala log10 para visualizar mejor las duraciones menos frecuentes",
    x = "Noches", y = "Cantidad de busquedas — log10"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))


## ----eda_mes_ci, fig.width=9, fig.height=4------------------------------------
meses <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

dataset_unido %>%
  count(month_ci) %>%
  mutate(
    mes_nombre = factor(meses[month_ci], levels = meses),
    pct = round(n / sum(n) * 100, 1)
  ) %>%
  ggplot(aes(x = mes_nombre, y = n, fill = mes_nombre)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.5, size = 3.2) +
  scale_fill_viridis_d(option = "turbo") +
  labs(
    title = "Busquedas por mes de check-in",
    x = "Mes de check-in", y = "Cantidad de busquedas"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))


## ----eda_correlacion, fig.width=10, fig.height=8------------------------------
vars_num <- dataset_unido %>%
  select(price_by_night, price_by_night_adult, price_by_night_person,
         min_query_price, starRating, avgRating, avgRatingCleaning,
         avgRatingLocation, avgQualityprice, avgService,
         anticipation, duration, numberOfRooms, target) %>%
  drop_na()

cor_matrix <- cor(vars_num)

corrplot(cor_matrix,
         method   = "color",
         type     = "upper",
         tl.cex   = 0.8,
         tl.col   = "black",
         addCoef.col = "black",
         number.cex  = 0.6,
         col      = colorRampPalette(c("#E63946", "white", "#457B9D"))(200),
         title    = "Matriz de correlacion - Variables numericas",
         mar      = c(0, 0, 2, 0))


## ----eda_posicion, fig.width=9, fig.height=4----------------------------------
dataset_unido %>%
  filter(position <= 30) %>%
  group_by(position) %>%
  summarise(
    tasa_compra = mean(target, na.rm = TRUE),
    n = n()
  ) %>%
  ggplot(aes(x = position, y = tasa_compra)) +
  geom_line(color = "#457B9D", linewidth = 1.2) +
  geom_point(aes(size = n), color = "#E63946", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Tasa de compra segun posicion del hotel en los resultados",
    subtitle = "Solo posiciones 1-30",
    x = "Posicion en resultados", y = "Tasa de compra", size = "N busquedas"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


## ----filtro_rio---------------------------------------------------------------
# geo_id = 6381 corresponde a Rio de Janeiro
df_rio <- dataset_unido %>% filter(geo_id == 6381)

cat("Filas totales en dataset_unido:", nrow(dataset_unido), "\n")


## ----variable_objetivo--------------------------------------------------------
# Usar price_by_night_person como variable objetivo
# Filtrar registros con precio <= 0 o NA
df_rio <- df_rio %>%
  filter(!is.na(price_by_night_person), price_by_night_person > 0)

cat("Filas luego de filtrar precio invalido:", nrow(df_rio), "\n")
cat("Min precio por noche por persona: $", min(df_rio$price_by_night_person), "\n")
cat("Max precio por noche por persona: $", max(df_rio$price_by_night_person), "\n")
cat("Mediana precio por noche por persona: $", median(df_rio$price_by_night_person), "\n")


## ----features_temporales------------------------------------------------------
# Tabla de feriados Brasil 2023-2024
feriados_brasil <- as.Date(c(
  # 2023
  "2023-01-01",  # Año Nuevo
  "2023-02-20", "2023-02-21",  # Carnaval
  "2023-04-07", "2023-04-09",  # Semana Santa (Viernes y Domingo)
  "2023-04-21",  # Tiradentes
  "2023-05-01",  # Dia del Trabajo
  "2023-06-08",  # Corpus Christi
  "2023-09-07",  # Independencia
  "2023-10-12",  # Nossa Senhora Aparecida
  "2023-11-02",  # Finados
  "2023-11-15",  # Proclamacion Republica
  "2023-12-25",  # Navidad
  # 2024
  "2024-01-01",  # Año Nuevo
  "2024-02-12", "2024-02-13",  # Carnaval
  "2024-03-29", "2024-03-31",  # Semana Santa
  "2024-04-21",  # Tiradentes
  "2024-05-01",  # Dia del Trabajo
  "2024-05-30",  # Corpus Christi
  "2024-09-07",  # Independencia
  "2024-10-12",  # Nossa Senhora Aparecida
  "2024-11-02",  # Finados
  "2024-11-15",  # Proclamacion Republica
  "2024-12-25"   # Navidad
))

df_rio <- df_rio %>%
  mutate(
    date_parsed  = as.Date(date),
    dia_semana   = wday(date_parsed, week_start = 1),       # 1=Lun ... 7=Dom
    dia_del_anio = yday(date_parsed),                        # 1-365 (tendencia lineal)
    es_finde     = ifelse(dia_semana %in% c(6, 7), 1, 0),   # fin de semana
    es_feriado   = ifelse(date_parsed %in% feriados_brasil, 1, 0)
  )

cat("Distribucion es_finde:\n"); print(table(df_rio$es_finde))
cat("Distribucion es_feriado:\n"); print(table(df_rio$es_feriado))


## ----precio_relativo----------------------------------------------------------
# Precio relativo = precio por persona / precio minimo de la busqueda
# Captura si el hotel es caro respecto al mercado en esa consulta
df_rio <- df_rio %>%
  mutate(
    precio_relativo = ifelse(
      !is.na(min_query_price) & min_query_price > 0,
      price_by_night_person / min_query_price,
      NA_real_
    )
  )

cat("Resumen precio_relativo:\n")
summary(df_rio$precio_relativo)


## ----features_geo-------------------------------------------------------------
df_rio <- df_rio %>%
  mutate(
    dist_copacabana_km = distHaversine(
      cbind(longitude, latitude),
      c(-43.1823, -22.9711)
    ) / 1000,
    dist_ipanema_km = distHaversine(
      cbind(longitude, latitude),
      c(-43.2096, -22.9838)
    ) / 1000,
    dist_cristo_km = distHaversine(
      cbind(longitude, latitude),
      c(-43.2105, -22.9519)
    ) / 1000,
    dist_centro_km = distHaversine(
      cbind(longitude, latitude),
      c(-43.1731, -22.9028)
    ) / 1000
  )

cat("Resumen distancias geograficas (km):\n")
df_rio %>%
  select(dist_copacabana_km, dist_ipanema_km, dist_cristo_km, dist_centro_km) %>%
  summary()


## ----plot_dist_precio, fig.width=10, fig.height=4-----------------------------
# Muestreamos 5000 filas para el grafico (LOESS es O(n^2), muy lento en datasets grandes)
# Los datos reales df_rio NO se modifican — solo se usa la muestra para visualizar la tendencia
set.seed(42)
df_plot_dist <- df_rio %>%
  filter(dist_copacabana_km < 20)
df_plot_dist <- df_plot_dist %>%
  slice_sample(n = min(5000, nrow(df_plot_dist)))

ggplot(df_plot_dist, aes(x = dist_copacabana_km, y = price_by_night_person)) +
  geom_point(alpha = 0.15, color = "#457B9D", size = 0.8) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              color = "#E63946", linewidth = 1.2, se = TRUE) +
  scale_y_log10(labels = scales::dollar_format()) +
  labs(
    title = "Precio por noche por persona vs. distancia a Copacabana",
    subtitle = "Relacion entre proximidad a la playa y precio del hotel (muestra de 5000 obs.)",
    x = "Distancia a Copacabana (km)", y = "Precio por noche por persona (USD) — log10"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))


## ----features_amenities-------------------------------------------------------
df_rio <- df_rio %>%
  mutate(
    tiene_estacionamiento = ifelse(!is.na(amenity_category.id) & amenity_category.id == "ESTA", 1, 0),
    tiene_servicios_com   = ifelse(!is.na(amenity_category.id) & amenity_category.id == "COMU", 1, 0),
    tiene_para_ninos      = ifelse(!is.na(amenity_category.id) & amenity_category.id == "NINO", 1, 0)
  )

cat("Amenities disponibles (categorias unicas):\n")
print(table(df_rio$amenity_category.id, useNA = "always"))

cat("\nResumen dummies de amenities:\n")
df_rio %>%
  select(tiene_estacionamiento, tiene_servicios_com, tiene_para_ninos) %>%
  summarise(across(everything(), sum)) %>%
  print()


## ----features_popularidad-----------------------------------------------------
busquedas_x_hotel <- df_rio %>%
  group_by(hid) %>%
  summarise(
    n_busquedas  = n(),
    tasa_compra  = mean(target, na.rm = TRUE),
    .groups = "drop"
  )

df_rio <- df_rio %>%
  left_join(busquedas_x_hotel, by = "hid")

cat("Resumen n_busquedas por hotel:\n")
summary(df_rio$n_busquedas)

cat("\nTop 10 hoteles mas buscados:\n")
busquedas_x_hotel %>%
  arrange(desc(n_busquedas)) %>%
  head(10) %>%
  print()


## ----outliers_precio----------------------------------------------------------
# --- Outliers en precio: metodo IQR ---
q1 <- quantile(df_rio$price_by_night_person, 0.25, na.rm = TRUE)
q3 <- quantile(df_rio$price_by_night_person, 0.75, na.rm = TRUE)
iqr <- q3 - q1
limite_sup <- q3 + 3 * iqr  # 3*IQR para no ser demasiado agresivo
limite_inf <- max(0, q1 - 3 * iqr)

n_antes <- nrow(df_rio)
df_rio <- df_rio %>%
  filter(price_by_night_person >= limite_inf, price_by_night_person <= limite_sup)
n_despues <- nrow(df_rio)

cat("Limite inferior IQR:", limite_inf, "\n")
cat("Limite superior IQR:", limite_sup, "\n")
cat("Filas eliminadas como outliers:", n_antes - n_despues, "\n")
cat("Filas restantes:", n_despues, "\n")


## ----imputacion_ratings-------------------------------------------------------
# --- Imputar NAs de ratings con mediana por starRating ---
ratings_vars <- c("avgRating", "avgRatingCleaning", "avgRatingInternetAccessAndQuality",
                  "avgRatingLocation", "avgQualityprice", "avgServicepersonal", "avgService")

df_rio <- df_rio %>%
  group_by(starRating) %>%
  mutate(across(
    all_of(ratings_vars),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  ungroup()

cat("NAs restantes en ratings luego de imputacion:\n")
df_rio %>%
  select(all_of(ratings_vars)) %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  print()


## ----resumen_df_rio-----------------------------------------------------------
cat("=== Dataset final df_rio ===\n")
cat("Filas:   ", nrow(df_rio), "\n")
cat("Columnas:", ncol(df_rio), "\n")

cat("\nNuevas features creadas:\n")
nuevas_features <- c(
  "date_parsed", "dia_semana", "dia_del_anio", "es_finde", "es_feriado",
  "precio_relativo",
  "dist_copacabana_km", "dist_ipanema_km", "dist_cristo_km", "dist_centro_km",
  "tiene_estacionamiento", "tiene_servicios_com", "tiene_para_ninos",
  "n_busquedas", "tasa_compra"
)
print(nuevas_features)

cat("\nResumen variable objetivo (price_by_night_person):\n")
summary(df_rio$price_by_night_person)


## ----limpieza_leakage---------------------------------------------------------
# ---------------------------------------------------------------
# Variables a eliminar por riesgo de data leakage:
#   - price_by_night        : precio total (derivacion directa del target)
#   - price_by_night_adult  : derivacion directa del target
#   - min_query_price       : precio minimo de la busqueda (contamina al ser base del precio relativo)
#   - precio_relativo       : ratio construido sobre el target (price_by_night_person / min_query_price)
#   - target                : variable objetivo de la Parte 1 (clasificacion), no relevante aqui
#
# Variables ignoradas por ser identificadores (no aportan informacion predictiva):
#   - hid  : ID del hotel
#   - id   : ID de la fila / busqueda
# ---------------------------------------------------------------

df_modelo <- df_rio %>%
  select(
    # --- Variable objetivo ---
    price_by_night_person,

    # --- Features geograficas ---
    latitude, longitude,
    dist_copacabana_km, dist_ipanema_km, dist_cristo_km, dist_centro_km,

    # --- Features del hotel ---
    starRating,
    avgRating, avgRatingCleaning, avgRatingInternetAccessAndQuality,
    avgRatingLocation, avgQualityprice, avgServicepersonal, avgService,
    numberOfRooms,

    # --- Features de la busqueda ---
    anticipation, duration, position,

    # --- Features temporales (creadas en seccion 6.3) ---
    dia_semana, dia_del_anio, es_finde, es_feriado,

    # --- Features de pais de origen del usuario (dummies) ---
    AR, BR, CL, CO, MX, PE, UY, OTHER,

    # --- Features de amenities (dummies) ---
    tiene_estacionamiento, tiene_servicios_com, tiene_para_ninos,

    # --- Features de popularidad del hotel ---
    n_busquedas, tasa_compra
  ) %>%
  # Eliminamos filas con NA en cualquier columna para garantizar un dataset limpio
  drop_na()

cat("=== Dataset listo para modelado ===\n")
cat("Filas:   ", nrow(df_modelo), "\n")
cat("Columnas:", ncol(df_modelo), "\n")
cat("\nColumnas incluidas:\n")
print(colnames(df_modelo))


## ----train_test_split---------------------------------------------------------
# Instalamos rsample si no esta disponible
if (!requireNamespace("rsample", quietly = TRUE)) install.packages("rsample")
library(rsample)

# Fijamos semilla para reproducibilidad
set.seed(42)

# Split estratificado: 80% train / 20% test
# strata asegura que la distribucion de precio sea similar en ambas particiones
split_obj <- initial_split(df_modelo, prop = 0.80, strata = price_by_night_person)

train_data <- training(split_obj)
test_data  <- testing(split_obj)

cat("Filas en entrenamiento:", nrow(train_data), "\n")
cat("Filas en prueba:       ", nrow(test_data),  "\n")
cat("\nMedia del precio en train: $", round(mean(train_data$price_by_night_person), 2), "\n")
cat("Media del precio en test:  $", round(mean(test_data$price_by_night_person),  2), "\n")


## ----correlacion_simple-------------------------------------------------------
# ---------------------------------------------------------------
# Calculamos la correlacion de Pearson entre cada feature numerica
# y la variable objetivo. Tomamos el valor absoluto para ordenar
# por magnitud sin importar el signo.
# ---------------------------------------------------------------
features_num <- train_data %>%
  select(-price_by_night_person) %>%
  select(where(is.numeric))

correlaciones <- cor(features_num, train_data$price_by_night_person, use = "complete.obs")

cor_df <- data.frame(
  variable    = rownames(correlaciones),
  correlacion = round(as.numeric(correlaciones), 4)
) %>%
  arrange(desc(abs(correlacion)))

cat("Top 10 variables con mayor correlacion con price_by_night_person:\n")
print(head(cor_df, 10))

# Identificamos la variable con mayor correlacion (en valor absoluto)
mejor_var <- cor_df$variable[1]
cat("\nVariable seleccionada para el modelo lineal simple:", mejor_var, "\n")
cat("Correlacion:", cor_df$correlacion[1], "\n")


## ----scatter_mejor_var, fig.width=9, fig.height=5-----------------------------
# ---------------------------------------------------------------
# Scatterplot: variable mas correlacionada vs. precio objetivo
# Se agrega la linea de regresion lineal (method = "lm")
# ---------------------------------------------------------------
ggplot(train_data, aes_string(x = mejor_var, y = "price_by_night_person")) +
  geom_point(alpha = 0.18, color = "#457B9D", size = 0.9) +
  geom_smooth(method = "lm", color = "#E63946", linewidth = 1.3,
              fill = "#E6394620", se = TRUE) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title    = paste0("Precio por noche por persona vs. ", mejor_var),
    subtitle = paste0("Correlacion de Pearson: r = ", cor_df$correlacion[1]),
    x        = mejor_var,
    y        = "Precio por noche por persona (USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40")
  )


## ----modelo_lineal_simple-----------------------------------------------------
# ---------------------------------------------------------------
# Modelo lineal simple: price_by_night_person ~ mejor_var
# Entrenado unicamente sobre el conjunto de entrenamiento
# ---------------------------------------------------------------
formula_simple <- as.formula(paste("price_by_night_person ~", mejor_var))
modelo_lm      <- lm(formula_simple, data = train_data)

cat("=== Resumen del modelo lineal simple (baseline) ===\n")
summary(modelo_lm)


## ----modelo_ranger------------------------------------------------------------
# Instalamos ranger si no esta disponible
if (!requireNamespace("ranger", quietly = TRUE)) {
  install.packages("ranger", repos = "https://cloud.r-project.org")
}
library(ranger)

set.seed(42)

# ---------------------------------------------------------------
# Parametros del modelo:
#   num.trees       = 200   : suficiente para estabilizar predicciones
#   mtry            = floor(sqrt(p)) : features candidatas por split (default RF)
#   sample.fraction = 0.3   : cada arbol entrena sobre el 30% del train set
#                             (subsampling nativo de ranger). Drasticamente mas
#                             rapido y estadisticamente valido para datasets grandes.
#   importance      = 'impurity' : reduccion de varianza acumulada por split.
#                             Se calcula de forma gratuita durante el entrenamiento.
#   num.threads     = nucleos disponibles - 1 : paralelismo para acelerar el fit
#   respect.unordered.factors = 'order' : manejo correcto de factores
# ---------------------------------------------------------------
n_features  <- ncol(train_data) - 1          # excluye la variable objetivo
n_cores     <- max(1, parallel::detectCores() - 1)  # usa todos los nucleos disponibles

modelo_rf <- ranger(
  formula                    = price_by_night_person ~ .,
  data                       = train_data,
  num.trees                  = 200,
  mtry                       = floor(sqrt(n_features)),
  sample.fraction            = 0.3,           # subsampling: 30% por arbol
  importance                 = "impurity",    # reduccion de varianza (rapido)
  respect.unordered.factors  = "order",
  num.threads                = n_cores,       # paralelismo
  seed                       = 42,
  verbose                    = FALSE
)

cat("=== Resumen del modelo Random Forest (ranger) ===\n")
print(modelo_rf)
cat("\nR-cuadrado en OOB (Out-of-Bag):", round(modelo_rf$r.squared, 4), "\n")
cat("Nucleos utilizados:", n_cores, "\n")


## ----evaluacion_metricas------------------------------------------------------
# ---------------------------------------------------------------
# Predicciones sobre el set de prueba
# ---------------------------------------------------------------
predicciones <- predict(modelo_rf, data = test_data)$predictions

# Valores reales del set de prueba
reales <- test_data$price_by_night_person

# Calculo de metricas
rmse <- sqrt(mean((reales - predicciones)^2))
mae  <- mean(abs(reales - predicciones))
ss_res <- sum((reales - predicciones)^2)
ss_tot <- sum((reales - mean(reales))^2)
r2   <- 1 - (ss_res / ss_tot)

cat("===========================================\n")
cat("   METRICAS DE EVALUACION — SET DE PRUEBA  \n")
cat("===========================================\n")
cat(sprintf("  RMSE : $ %.2f\n", rmse))
cat(sprintf("  MAE  : $ %.2f\n", mae))
cat(sprintf("  R²   :   %.4f (%.1f%% de varianza explicada)\n", r2, r2 * 100))
cat("===========================================\n")

# Comparativa con el baseline (modelo lineal simple)
pred_lm  <- predict(modelo_lm, newdata = test_data)
rmse_lm  <- sqrt(mean((reales - pred_lm)^2))
mae_lm   <- mean(abs(reales - pred_lm))
r2_lm    <- 1 - sum((reales - pred_lm)^2) / ss_tot

cat("\nComparativa con modelo lineal simple (baseline):\n")
cat(sprintf("  RMSE baseline : $ %.2f  |  RMSE RF : $ %.2f\n", rmse_lm, rmse))
cat(sprintf("  MAE  baseline : $ %.2f  |  MAE  RF : $ %.2f\n", mae_lm,  mae))
cat(sprintf("  R²   baseline :   %.4f  |  R²   RF :   %.4f\n", r2_lm,   r2))


## ----plot_reales_vs_pred, fig.width=9, fig.height=6---------------------------
# ---------------------------------------------------------------
# Grafico 1: Valores Reales vs. Predicciones
# La linea punteada de 45 grados representa la prediccion perfecta.
# Puntos sobre la linea = subestimacion; debajo = sobreestimacion.
# ---------------------------------------------------------------
resultados_df <- data.frame(
  real       = reales,
  prediccion = predicciones
)

lim_max <- max(max(reales), max(predicciones)) * 1.05  # eje identico en X e Y

ggplot(resultados_df, aes(x = real, y = prediccion)) +
  # Linea de prediccion perfecta (45 grados)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "#E63946", linewidth = 1, alpha = 0.8) +
  # Puntos con transparencia para ver densidad
  geom_point(alpha = 0.25, color = "#457B9D", size = 1.5) +
  # Tendencia lineal (lm es instantaneo; loess es O(n^2) y muy lento en datasets grandes)
  geom_smooth(method = "lm", color = "#2DC653", linewidth = 1.1, se = FALSE) +
  scale_x_continuous(labels = scales::dollar_format(), limits = c(0, lim_max)) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, lim_max)) +
  annotate("text", x = lim_max * 0.05, y = lim_max * 0.95,
           label = paste0("R² = ", round(r2, 3), "\nRMSE = $", round(rmse, 1)),
           hjust = 0, vjust = 1, size = 4, color = "grey30", fontface = "bold") +
  labs(
    title    = "Valores Reales vs. Predicciones — Random Forest",
    subtitle = "La linea roja punteada representa la prediccion perfecta (45°)",
    x        = "Precio real (USD)",
    y        = "Precio predicho (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, color = "#1D3557"),
    plot.subtitle = element_text(color = "grey45", size = 11),
    panel.grid.minor = element_blank()
  )


## ----plot_feature_importance, fig.width=10, fig.height=6----------------------
# ---------------------------------------------------------------
# Grafico 2: Top 10 variables mas importantes (Feature Importance)
# Se usa la importancia por permutacion: cuanto aumenta el error
# del modelo cuando se permutan aleatoriamente los valores de esa variable.
# Una importancia alta significa que la variable es critica para el modelo.
# ---------------------------------------------------------------

# Extraemos la importancia y tomamos el Top 10
importancia_df <- data.frame(
  variable    = names(modelo_rf$variable.importance),
  importancia = modelo_rf$variable.importance
) %>%
  arrange(desc(importancia)) %>%
  head(10) %>%
  mutate(
    variable = factor(variable, levels = rev(variable)),  # orden para coord_flip
    # Paleta de colores gradiente segun importancia
    color_rank = row_number()
  )

ggplot(importancia_df, aes(x = variable, y = importancia, fill = color_rank)) +
  geom_col(alpha = 0.92, width = 0.72) +
  geom_text(aes(label = round(importancia, 2)), hjust = -0.12,
            fontface = "bold", size = 3.5, color = "grey25") +
  coord_flip() +
  scale_fill_gradient(low = "#A8DADC", high = "#1D3557", guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Top 10 Variables mas Importantes — Random Forest",
    subtitle = "Importancia por permutacion: aumento del error MSE al permutar cada variable",
    x        = NULL,
    y        = "Importancia (incremento en MSE)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, color = "#1D3557"),
    plot.subtitle    = element_text(color = "grey45", size = 11),
    axis.text.y      = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )


cat('=== FINAL METRICS ===\n')
print(metricas_rf)
cat('=== FEATURE IMPORTANCE ===\n')
print(head(importancia_rf, 5))

