# CGR PRENSA — PASO 5: modelado de temas (topic modeling con {stm})
# Descubre los temas latentes de la cobertura de la CGR a partir de
# título + bajada + cuerpo (cuando existe). Permite responder "¿de qué se
# está hablando?" en vez de "¿qué palabras aparecen?".
#
# Decisiones:
#   - K = 8 temas (fijo, en el rango 6-10 sugerido para corpus de este tamaño).
#   - Texto normalizado (minúsculas + sin tildes) para alinearse con el resto
#     del pipeline.
#   - Stopwords = español de tm + propias + dominio CGR (palabras que
#     aparecen en TODOS los artículos no informan, ej. "contraloria",
#     "contralor", "cgr").
#   - Etiqueta inicial = top 3 términos por β. La etiqueta legible final se
#     puede sobreescribir con la pasada LLM (ver Fase 3).
#
# input : datos/cgr_datos.parquet
# output: datos/cgr_temas_terminos.parquet (tema, etiqueta, palabra, beta)
#         datos/cgr_temas_doc.parquet      (id, tema, etiqueta, gamma)
#         datos/cgr_temas_semana.parquet   (semana, tema, etiqueta, prevalencia)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(arrow); library(lubridate)
  library(stm); library(tm)
})
source("funciones.R")
source("datos/cgr_terminos.R")  # normalizar_texto + stopwords_propias

tini <- Sys.time()

K_TEMAS <- 8
SEMILLA <- 2026

if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/cgr_datos.parquet")

# 1. Construir el texto y la metadata --------------------------------------------
# Usa cuerpo cuando lo hay; si no, título + bajada (paywall/headline only).
# Normaliza acentos y minúsculas para empatar con cgr_palabras.
corpus <- datos_prensa |>
  mutate(
    cuerpo_limpio = tidyr::replace_na(cuerpo_limpio, ""),
    bajada = tidyr::replace_na(bajada, ""),
    texto = str_squish(paste(titulo, bajada, cuerpo_limpio)),
    texto = normalizar_texto(texto)
  ) |>
  filter(nchar(texto) >= 60) |>
  select(id, fuente, semana, fecha, texto)

message(glue::glue("Documentos para modelar: {nrow(corpus)}"))

# 2. Stopwords ES + dominio CGR --------------------------------------------------
sw_es <- normalizar_texto(tm::stopwords("spanish"))
sw_dom <- normalizar_texto(c(
  # palabras del dominio CGR que aparecen en TODAS las notas y no segmentan
  "contraloria", "contralor", "contralora", "cgr", "general", "republica",
  # alta frecuencia genérica de noticia
  "indico", "indica", "senalo", "senala", "afirmo", "afirma", "sostuvo",
  "explico", "agrego", "aseguro", "manifesto", "expreso", "comento",
  "detallo", "dijo", "dice", "advirtio", "sostuvo", "tras", "ademas",
  "ano", "anos", "dia", "dias", "hoy", "ayer", "mientras", "luego",
  "pais", "chile", "chilena", "nacional", "millones", "mil",
  # palabras vacías que siguen filtrándose
  "solo", "asi", "asimismo", "asimismo", "tambien", "luego", "tal", "mismo",
  "misma", "uno", "dos", "tres", "cuatro", "cinco", "primer", "segunda",
  "tercera", "ultimo", "ultima", "hizo", "hace", "haciendo", "sera", "sera"
))
sw_propias <- normalizar_texto(stopwords_propias)
custom_sw <- unique(c(sw_es, sw_dom, sw_propias))

# 3. Pre-procesar con stm (lower/strip ya hechos; sin stemming para legibilidad)
meta <- corpus |> select(id, fuente, semana, fecha) |> as.data.frame()
proc <- textProcessor(
  documents       = corpus$texto,
  metadata        = meta,
  lowercase       = FALSE,
  removestopwords = FALSE,            # usamos customstopwords (más control)
  removenumbers   = TRUE,
  removepunctuation = TRUE,
  stem            = FALSE,
  customstopwords = custom_sw,
  language        = "es",
  striphtml       = FALSE,
  verbose         = FALSE
)
out <- prepDocuments(proc$documents, proc$vocab, proc$meta,
                     lower.thresh = 3, verbose = FALSE)
message(glue::glue("Tras prep: {length(out$documents)} docs, {length(out$vocab)} términos"))

# 4. Ajustar STM -----------------------------------------------------------------
set.seed(SEMILLA)
mod <- stm(documents = out$documents, vocab = out$vocab, K = K_TEMAS,
           data = out$meta, init.type = "Spectral",
           max.em.its = 75, seed = SEMILLA, verbose = FALSE)

# 5. Términos por tema (tidy beta) + etiqueta = top 3 términos ------------------
beta_tidy <- tidytext::tidy(mod, matrix = "beta")  # cols: topic, term, beta

terminos <- beta_tidy |>
  group_by(topic) |>
  slice_max(beta, n = 12, with_ties = FALSE) |>
  ungroup() |>
  rename(tema = topic, palabra = term)

etiquetas <- terminos |>
  group_by(tema) |>
  arrange(desc(beta)) |>
  slice_head(n = 3) |>
  summarise(etiqueta = paste(palabra, collapse = " · "), .groups = "drop")

terminos <- terminos |> left_join(etiquetas, by = "tema") |>
  select(tema, etiqueta, palabra, beta)

arrow::write_parquet(terminos, "datos/cgr_temas_terminos.parquet")

# 6. Proporción de cada tema por documento (gamma = theta) ----------------------
theta <- as.data.frame(mod$theta)
names(theta) <- paste0("T", seq_len(ncol(theta)))
doc_tema <- bind_cols(out$meta, theta) |>
  pivot_longer(starts_with("T"), names_to = "tema_str", values_to = "gamma") |>
  mutate(tema = as.integer(sub("T", "", tema_str))) |>
  select(-tema_str) |>
  left_join(etiquetas, by = "tema") |>
  select(id, fuente, semana, fecha, tema, etiqueta, gamma)

arrow::write_parquet(doc_tema, "datos/cgr_temas_doc.parquet")

# 7. Prevalencia semanal de cada tema (promedio de gamma) -----------------------
temas_semana <- doc_tema |>
  group_by(semana, tema, etiqueta) |>
  summarise(prevalencia = mean(gamma), n_doc = n_distinct(id), .groups = "drop") |>
  arrange(semana, tema)

arrow::write_parquet(temas_semana, "datos/cgr_temas_semana.parquet")

# resumen
message(glue::glue(
  "P5 (temas) listo en {round(difftime(Sys.time(), tini, units='secs'),1)} s\n",
  "  K = {K_TEMAS} | {nrow(terminos)} (tema,palabra) | {nrow(doc_tema)} (doc,tema)"
))
print(etiquetas)
