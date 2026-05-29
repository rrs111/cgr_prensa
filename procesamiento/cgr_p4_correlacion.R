# CGR PRENSA — PASO 4: correlación entre palabras
# Calcula qué palabras tienden a aparecer juntas dentro de una misma noticia,
# usando {widyr}::pairwise_cor() (coeficiente phi sobre presencia por noticia).
#
# input : datos/cgr_palabras.parquet (paso 2)
# output: datos/cgr_correlacion.parquet  (item1, item2, correlation)

suppressPackageStartupMessages({
  library(dplyr)
  library(widyr)
  library(arrow)
})
source("funciones.R")

tini <- Sys.time()

if (!exists("palabras")) palabras <- arrow::read_parquet("datos/cgr_palabras.parquet")

corr_min   <- 0.15      # correlación mínima a conservar
max_vocab  <- 400       # tope de palabras a correlacionar (las más frecuentes)
max_pares  <- 25        # tope de asociaciones por palabra (limita el tamaño)

# presencia de palabra por noticia ----------------------------------------------
conteo <- palabras |>
  distinct(id, palabra) |>
  add_count(palabra, name = "freq")

n_docs <- n_distinct(conteo$id)

# vocabulario: palabras presentes en varias noticias y, de ellas, las más
# frecuentes (evita correlaciones espurias y acota el tamaño del resultado).
umbral <- max(4, ceiling(n_docs * 0.03))
vocab <- conteo |>
  distinct(palabra, freq) |>
  filter(freq >= umbral) |>
  slice_max(freq, n = max_vocab, with_ties = FALSE) |>
  pull(palabra)

conteo <- conteo |> filter(palabra %in% vocab) |> select(id, palabra)

message(glue::glue("Palabras a correlacionar: {length(vocab)} (umbral presencia: {umbral} de {n_docs} noticias)"))

correlacion <- tibble::tibble(item1 = character(), item2 = character(), correlation = numeric())
if (length(vocab) >= 2) {
  correlacion <- conteo |>
    pairwise_cor(palabra, id, sort = TRUE) |>
    filter(!is.na(correlation), correlation >= corr_min) |>
    group_by(item1) |>
    slice_max(correlation, n = max_pares, with_ties = FALSE) |>
    ungroup()
}

arrow::write_parquet(correlacion, "datos/cgr_correlacion.parquet")

message(glue::glue("P4 listo en {round(difftime(Sys.time(), tini, units='secs'),1)} s — ",
                   "{nrow(correlacion)} pares — datos/cgr_correlacion.parquet"))
