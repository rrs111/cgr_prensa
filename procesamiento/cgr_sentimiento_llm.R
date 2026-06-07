# CGR PRENSA — POSTURA HACIA LA CGR con LLM
# Un LLM LEE cada noticia y juzga si deja a la Contraloría / contralora en
# posición FAVORABLE / NEUTRA / DESFAVORABLE, entendiendo el contexto (a
# diferencia del tono por léxico de cgr_p4_sentimiento.R, que solo cuenta
# palabras y se sesga positivo por el vocabulario institucional).
#
# Soporta dos backends (variable CGR_LLM_BACKEND):
#   - "gemini" (def.): API de Google Gemini. Gratis, no ocupa tu computador
#                      (solo requests HTTP). Requiere GEMINI_API_KEY.
#   - "ollama"       : modelo Llama local vía Ollama (offline, pero usa tu CPU/RAM).
#
# Caché incremental: solo clasifica las noticias sin postura previa.
#
# === Cómo conseguir la API key de Gemini (gratis, sin tarjeta) ===
#   1. https://aistudio.google.com/apikey  → "Create API key"
#   2. Guardala (NO en el código). Opciones:
#        a) archivo .Renviron en la raíz del proyecto (está gitignored):
#             GEMINI_API_KEY=AIza...
#        b) o exportala en tu shell:  export GEMINI_API_KEY=AIza...
#
# Uso:  Rscript procesamiento/cgr_sentimiento_llm.R
# Luego: git add datos/cgr_postura_*.parquet && git commit && git push
#
# Variables: CGR_LLM_BACKEND, CGR_LLM_MODELO, CGR_LLM_MAX (tope por corrida)
#
# input : datos/cgr_datos.parquet
# output: datos/cgr_postura_articulo.parquet (id, postura)
#         datos/cgr_postura_semana.parquet   (semana, n, pct_desfavorable, ...)
#         datos/cgr_postura_fuente.parquet   (fuente, n, pct_desfavorable, ...)

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(stringi); library(tidyr)
  library(arrow); library(lubridate); library(httr2)
})

if (file.exists(".Renviron")) readRenviron(".Renviron")

BACKEND <- Sys.getenv("CGR_LLM_BACKEND", "gemini")
tini <- Sys.time()

norm <- function(x) tolower(stri_trans_general(x, "Latin-ASCII"))

# Mapea la respuesta libre del modelo a una de las 3 clases.
# "desfavorable" se chequea PRIMERO porque contiene "favorable" como substring.
mapear_postura <- function(r) {
  r <- norm(r %||% "")
  if (str_detect(r, "desfavorable")) return("desfavorable")
  if (str_detect(r, "favorable"))    return("favorable")
  if (str_detect(r, "neutr"))        return("neutra")
  "neutra"
}

INSTRUCCION <- paste(
  "Eres analista de medios chilenos. Lee la NOTICIA y responde con UNA sola",
  "palabra si deja a la Contraloria General de la Republica (CGR) o a su",
  "contralora Dorothy Perez en posicion: favorable, neutra o desfavorable.",
  "Si la CGR o la contralora es cuestionada, criticada, o aparece en una",
  "polemica, escandalo o conflicto: responde desfavorable.",
  "Si la CGR detecta irregularidades de otros, fiscaliza correctamente o se la",
  "destaca: responde favorable.",
  "Si solo se la menciona sin juzgarla: responde neutra."
)

armar_prompt <- function(texto) {
  paste0(INSTRUCCION, "\n\nNOTICIA:\n", texto, "\n\nRespuesta (una palabra):")
}

# --- Backend: Gemini --------------------------------------------------------
GEMINI_KEY    <- Sys.getenv("GEMINI_API_KEY", "")
GEMINI_MODELO <- Sys.getenv("CGR_LLM_MODELO", "gemini-flash-latest")
GEMINI_PAUSA  <- as.numeric(Sys.getenv("CGR_GEMINI_PAUSA", "1.2"))  # respeta rate limit

clasificar_gemini <- function(texto) {
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                GEMINI_MODELO, ":generateContent")
  cuerpo <- list(
    contents = list(list(parts = list(list(text = armar_prompt(texto))))),
    # thinkingBudget=0 desactiva el "razonamiento" (gemini-flash-latest es un
    # modelo con thinking que, sin esto, gasta el presupuesto de tokens pensando
    # y no alcanza a responder). maxOutputTokens holgado para la palabra.
    generationConfig = list(temperature = 0, maxOutputTokens = 20,
                            thinkingConfig = list(thinkingBudget = 0))
  )
  r <- tryCatch(
    request(url) |>
      req_url_query(key = GEMINI_KEY) |>
      req_body_json(cuerpo) |>
      req_timeout(40) |>
      req_retry(max_tries = 3, backoff = ~ 2 * .x) |>   # reintenta ante 429/5xx
      req_perform() |>
      resp_body_json(),
    error = function(e) NULL
  )
  if (is.null(r)) return(NA_character_)
  txt <- tryCatch(r$candidates[[1]]$content$parts[[1]]$text, error = function(e) NULL)
  if (is.null(txt)) return(NA_character_)
  mapear_postura(txt)
}

# --- Backend: Ollama local --------------------------------------------------
OLLAMA_URL    <- Sys.getenv("CGR_OLLAMA_URL", "http://localhost:11434/api/generate")
OLLAMA_MODELO <- Sys.getenv("CGR_LLM_MODELO", "llama3.2:3b")

clasificar_ollama <- function(texto) {
  r <- tryCatch(
    request(OLLAMA_URL) |>
      req_body_json(list(model = OLLAMA_MODELO, prompt = armar_prompt(texto),
                         stream = FALSE, options = list(temperature = 0, seed = 1))) |>
      req_timeout(90) |> req_perform() |> resp_body_json(),
    error = function(e) NULL
  )
  if (is.null(r) || is.null(r$response)) return(NA_character_)
  mapear_postura(r$response)
}

# --- Selección de backend + chequeo -----------------------------------------
if (BACKEND == "gemini") {
  if (!nzchar(GEMINI_KEY)) {
    stop("Falta GEMINI_API_KEY. Conseguila gratis en https://aistudio.google.com/apikey\n",
         "y ponela en .Renviron (GEMINI_API_KEY=AIza...) o export GEMINI_API_KEY=...")
  }
  clasificar <- clasificar_gemini
  pausa <- GEMINI_PAUSA
  modelo_txt <- GEMINI_MODELO
} else {
  clasificar <- clasificar_ollama
  pausa <- 0
  modelo_txt <- OLLAMA_MODELO
}

# 1. Datos + caché incremental -----------------------------------------------
datos <- arrow::read_parquet("datos/cgr_datos.parquet")
cache <- if (file.exists("datos/cgr_postura_articulo.parquet")) {
  arrow::read_parquet("datos/cgr_postura_articulo.parquet")
} else tibble(id = character(), postura = character())

pendientes <- datos |> filter(!id %in% cache$id)
lim <- suppressWarnings(as.integer(Sys.getenv("CGR_LLM_MAX", "")))
if (!is.na(lim)) pendientes <- head(pendientes, lim)

message(glue::glue("Backend: {BACKEND} ({modelo_txt}) | total: {nrow(datos)} | ",
                   "en caché: {nrow(cache)} | a clasificar: {nrow(pendientes)}"))

# 2. Clasificar pendientes ----------------------------------------------------
nuevas <- tibble(id = character(), postura = character())
if (nrow(pendientes) > 0) {
  textos <- pendientes |>
    mutate(texto = str_squish(paste(titulo, coalesce(bajada, ""),
                                    substr(coalesce(cuerpo, ""), 1, 1800))))
  posturas <- character(nrow(textos))
  for (i in seq_len(nrow(textos))) {
    posturas[i] <- clasificar(textos$texto[i])
    if (pausa > 0) Sys.sleep(pausa)
    if (i %% 25 == 0) message(glue::glue("  {i}/{nrow(textos)}…"))
  }
  nuevas <- tibble(id = textos$id, postura = replace_na(posturas, "neutra"))
}

# 3. Guardar caché + agregados -----------------------------------------------
postura_articulo <- bind_rows(cache, nuevas) |> distinct(id, .keep_all = TRUE)
arrow::write_parquet(postura_articulo, "datos/cgr_postura_articulo.parquet")

meta <- datos |> select(id, fuente, semana)
pa <- postura_articulo |> inner_join(meta, by = "id")

pa |>
  group_by(semana) |>
  summarise(n = n(),
            pct_desfavorable = round(mean(postura == "desfavorable"), 3),
            pct_neutra       = round(mean(postura == "neutra"), 3),
            pct_favorable    = round(mean(postura == "favorable"), 3),
            .groups = "drop") |>
  arrange(semana) |>
  arrow::write_parquet("datos/cgr_postura_semana.parquet")

pa |>
  group_by(fuente) |>
  summarise(n = n(),
            pct_desfavorable = round(mean(postura == "desfavorable"), 3),
            pct_favorable    = round(mean(postura == "favorable"), 3),
            .groups = "drop") |>
  filter(n >= 3) |>
  arrange(desc(pct_desfavorable)) |>
  arrow::write_parquet("datos/cgr_postura_fuente.parquet")

message(glue::glue(
  "Postura LLM lista en {round(difftime(Sys.time(), tini, units='mins'),1)} min — ",
  paste(names(table(postura_articulo$postura)), table(postura_articulo$postura),
        sep = "=", collapse = ", ")
))
