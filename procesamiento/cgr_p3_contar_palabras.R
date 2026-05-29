# CGR PRENSA — PASO 3: conteos de palabras
# A partir de la base tokenizada calcula:
#  - frecuencia de palabras por semana y por fuente (para tendencias y top words)
#  - noticias por semana y por fuente (para la pestaña Resumen)
#  - TF-IDF por fuente (palabras distintivas de cada medio)
#
# input : datos/cgr_palabras.parquet (paso 2), datos/cgr_datos.parquet (paso 1)
# output: datos/cgr_palabras_semana.parquet
#         datos/cgr_noticias_semana.parquet
#         datos/cgr_tfidf_fuente.parquet

suppressPackageStartupMessages({
  library(dplyr)
  library(tidytext)
  library(arrow)
})
source("funciones.R")

tini <- Sys.time()

if (!exists("palabras"))     palabras     <- arrow::read_parquet("datos/cgr_palabras.parquet")
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/cgr_datos.parquet")

# 1. Frecuencia de palabras por semana y fuente ---------------------------------
palabras_semana <- palabras |>
  count(semana, fuente, palabra, name = "n") |>
  arrange(desc(semana), desc(n))

arrow::write_parquet(palabras_semana, "datos/cgr_palabras_semana.parquet")

# 2. Noticias por semana y fuente -----------------------------------------------
noticias_semana <- datos_prensa |>
  count(semana, fuente, name = "n_noticias") |>
  arrange(desc(semana))

arrow::write_parquet(noticias_semana, "datos/cgr_noticias_semana.parquet")

# 3. TF-IDF por fuente ----------------------------------------------------------
conteo_fuente <- palabras |>
  count(fuente, palabra, name = "n") |>
  filter(n >= 2)

tfidf_fuente <- conteo_fuente |>
  bind_tf_idf(term = palabra, document = fuente, n = n) |>
  arrange(fuente, desc(tf_idf))

arrow::write_parquet(tfidf_fuente, "datos/cgr_tfidf_fuente.parquet")

# resumen
message(glue::glue(
  "P3 listo en {round(difftime(Sys.time(), tini, units='secs'),1)} s\n",
  "  palabras_semana: {nrow(palabras_semana)} filas\n",
  "  noticias_semana: {nrow(noticias_semana)} filas\n",
  "  tfidf_fuente:    {nrow(tfidf_fuente)} filas"
))
