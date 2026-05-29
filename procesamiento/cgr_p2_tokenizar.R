# CGR PRENSA — PASO 2: tokenización
# Transforma las noticias en una base de una palabra por fila. Quita stopwords
# (español + propias del dominio CGR), filtra por longitud, y agrupa por raíz
# (stemming con {SnowballC}) eligiendo la forma superficial más frecuente como
# representante (así "fiscaliza", "fiscalizar", "fiscalización" -> una sola).
#
# input : datos/cgr_datos.parquet  (paso 1)
# output: datos/cgr_palabras.parquet

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidytext)
  library(SnowballC)
  library(arrow)
})
source("funciones.R")
source("datos/cgr_terminos.R")

tini <- Sys.time()

# datos -------------------------------------------------------------------------
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/cgr_datos.parquet")

# stopwords ---------------------------------------------------------------------
stopwords_es <- if (file.exists("datos/stopwords_es.txt")) {
  readLines("datos/stopwords_es.txt", warn = FALSE)
} else {
  normalizar_texto(stopwords::stopwords("es", source = "snowball"))
}
stopwords_todas <- unique(c(stopwords_es, normalizar_texto(stopwords_propias)))

# tokenización ------------------------------------------------------------------
palabras <- datos_prensa |>
  select(id, fuente, fecha, semana, anio, texto_limpio) |>
  unnest_tokens(output = palabra, input = texto_limpio, token = "words") |>
  # normalizar (minúsculas, sin tildes) para comparar con stopwords y agrupar
  mutate(palabra = normalizar_texto(palabra)) |>
  filter(
    !palabra %in% stopwords_todas,
    !str_detect(palabra, "[0-9]"),
    nchar(palabra) >= 3,
    nchar(palabra) < 23
  )

message(glue::glue("Tokens tras stopwords/longitud: {nrow(palabras)}"))

# stemming + representante ------------------------------------------------------
# raíz de cada palabra
palabras <- palabras |>
  mutate(raiz = SnowballC::wordStem(palabra, language = "spanish"))

# elegir forma representante por raíz: la más frecuente (desempate: más corta)
representantes <- palabras |>
  count(raiz, palabra, name = "n") |>
  arrange(raiz, desc(n), nchar(palabra)) |>
  group_by(raiz) |>
  slice(1) |>
  ungroup() |>
  select(raiz, palabra_repr = palabra)

palabras <- palabras |>
  left_join(representantes, by = "raiz") |>
  mutate(palabra = coalesce(palabra_repr, palabra)) |>
  select(-palabra_repr)

# agrupaciones manuales del dominio CGR (unifican conceptos clave) --------------
palabras <- palabras |>
  mutate(palabra = case_match(
    palabra,
    c("contralor", "contralora", "contraloria", "contralorias", "contraloral") ~ "contraloria",
    c("dictamen", "dictamenes", "dictaminar", "dictamin") ~ "dictamen",
    c("fiscalizar", "fiscalizacion", "fiscalizaciones", "fiscaliz", "fiscalizador") ~ "fiscalizacion",
    c("auditoria", "auditorias", "auditar", "auditor") ~ "auditoria",
    c("sumario", "sumarios", "sumariar") ~ "sumario",
    c("irregularidad", "irregularidades", "irregular") ~ "irregularidad",
    c("corrupcion", "corrupto", "corruptos", "corrupta") ~ "corrupcion",
    c("probidad") ~ "probidad",
    c("transparencia") ~ "transparencia",
    c("malversacion", "malversar", "malvers") ~ "malversacion",
    c("publico", "publica", "publicos", "publicas") ~ "publico",
    c("funcionario", "funcionarios", "funcionaria", "funcionarias") ~ "funcionario",
    c("fondo", "fondos") ~ "fondos",
    c("recurso", "recursos") ~ "recursos",
    .default = palabra
  ))

# guardar -----------------------------------------------------------------------
arrow::write_parquet(palabras, "datos/cgr_palabras.parquet")

n_palabras <- nrow(palabras)
message(glue::glue("P2 listo en {round(difftime(Sys.time(), tini, units='secs'),1)} s — ",
                   "{n_palabras} tokens — datos/cgr_palabras.parquet"))
