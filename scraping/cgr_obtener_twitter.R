# CGR Prensa — Tuits sobre la Contraloría desde X/Twitter
# Soporta DOS backends (se elige según qué credencial esté definida):
#
#  1) ScrapeBadger (PREFERIDO si hay SCRAPEBADGER_API_KEY en el entorno o en
#     .Renviron). Endpoint /v1/twitter/tweets/advanced_search: hasta 100 tuits
#     por request, sintaxis de búsqueda de Twitter, ~1 crédito por tuit.
#     Sin tope mensual de lectura (solo el saldo de créditos de la cuenta).
#
#  2) API oficial de X v2 (X_BEARER_TOKEN). Plan gratuito: ~100 tuits LEÍDOS
#     al mes y 1 request/15 min, ventana de 7 días. Queda como respaldo.
#
# PRESUPUESTO: este módulo NO corre con el orquestador por defecto (gastaría
# créditos/cuota con el bot diario). Correrlo ~1 vez por semana:
#
#   Rscript scraping/cgr_obtener_twitter.R
#   (o CGR_FUENTES="twitter" Rscript scraping/cgr_scraping.R)
#
# Variables de entorno:
#   SCRAPEBADGER_API_KEY  key de scrapebadger.com (mejor en .Renviron,
#                         que está en .gitignore: el repo es público)
#   X_BEARER_TOKEN        token Bearer de developer.x.com (respaldo)
#   CGR_TW_MAX            tuits a pedir por corrida (def. 50 ScrapeBadger,
#                         25 API oficial; ambas APIs aceptan hasta 100)
#   CGR_TW_CUOTA          cuota mensual de lectura del plan oficial (def. 100)
#
# El resultado se ACUMULA en datos/cgr_tweets.parquet (dedup por id) y la app
# lo muestra en la pestaña Twitter/X. También guarda el .rds crudo de cada
# corrida en scraping/datos/twitter/, como los demás módulos.

source("funciones.R")

suppressPackageStartupMessages({
  library(httr2)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(glue)
})

SB_KEY  <- Sys.getenv("SCRAPEBADGER_API_KEY", "")
TOKEN_X <- Sys.getenv("X_BEARER_TOKEN", Sys.getenv("TWITTER_BEARER_TOKEN", ""))
RUTA_TUITS <- "datos/cgr_tweets.parquet"

# Mismos términos núcleo del monitor (datos/cgr_terminos.R), en sintaxis de
# búsqueda de Twitter: con y sin tilde por seguridad, sin retuits y solo
# español. OJO: varios países tienen Contraloría (Colombia, Perú, Panamá,
# Ecuador, Costa Rica, Guatemala...) y muchos tuits extranjeros NO nombran su
# país, así que el segundo grupo EXIGE señal chilena explícita (la mención a
# @Contraloriacl —cuenta oficial CGR— es la más fuerte). Además se aplica el
# filtro fino filtrar_chile() sobre el texto como defensa.
TERMINOS_TW <- paste(
  '(contraloria OR contraloría OR contralor OR contralora',
  'OR "dorothy perez" OR "dorothy pérez" OR @Contraloriacl)'
)
ANCLA_CHILE <- paste(
  "(chile OR chilena OR chileno OR dorothy OR @Contraloriacl",
  "OR boric OR hermosilla)"
)
# Sin retuits NI respuestas: las respuestas de hilos virales dominan el
# volumen y aportan poco al monitoreo (un solo hilo puede copar la muestra).
CONSULTA_SB <- paste(TERMINOS_TW, ANCLA_CHILE,
                     "-filter:retweets -filter:replies lang:es")
CONSULTA_X  <- paste(TERMINOS_TW, ANCLA_CHILE, "-is:retweet -is:reply lang:es")

# Filtro Chile: descarta tuits con marcadores de contralorías de OTROS países,
# salvo que el tuit también tenga señal chilena explícita.
filtrar_chile <- function(d) {
  if (nrow(d) == 0) return(d)
  norm <- d$texto |> stringi::stri_trans_general("Latin-ASCII") |> tolower()
  extranjero <- str_detect(norm, paste0(
    "colombia|bogot|petro|cgr_colombia|pgn_col|contralori?a(gen)?col|",
    "\\bperu\\b|contraloriaperu|costa rica|\\bpanama\\b|venezuela|maduro|",
    "\\becuador\\b|paraguay|\\bbolivia\\b|guatemala|honduras|nicaragua|",
    "el salvador|\\bmexico\\b|republica dominicana"
  ))
  chileno <- str_detect(norm, "dorothy|contraloriacl|\\bchile\\b|chilen")
  fuera <- extranjero & !chileno
  if (any(fuera)) message(glue("  filtro Chile: descartados {sum(fuera)} tuits de otros países"))
  d[!fuera, ]
}

leer_previos <- function() {
  if (!file.exists(RUTA_TUITS)) return(NULL)
  tryCatch(arrow::read_parquet(RUTA_TUITS), error = function(e) NULL)
}

# Acumula la corrida en el parquet que lee la app (dedup por id, nuevo primero)
acumular_tuits <- function(tuits, previos) {
  revisar_scraping(tuits, "twitter")
  if (is.null(tuits) || nrow(tuits) == 0) return(invisible(NULL))
  saveRDS(tuits, ruta_resultado("twitter"))
  acumulado <- bind_rows(previos, tuits) |>
    distinct(id, .keep_all = TRUE) |>
    distinct(autor, texto, .keep_all = TRUE) |>   # mismo texto re-posteado
    arrange(desc(fecha_hora))
  arrow::write_parquet(acumulado, RUTA_TUITS)
  message(glue("  acumulado: {nrow(acumulado)} tuits en {RUTA_TUITS}"))
  invisible(tuits)
}

# —--------------------------------------------------------------------------
# Backend 1: ScrapeBadger (advanced_search)
# —--------------------------------------------------------------------------
obtener_tweets_scrapebadger <- function() {
  message("\n===== twitter (ScrapeBadger) =====")
  pedido <- suppressWarnings(as.integer(Sys.getenv("CGR_TW_MAX", "50")))
  if (is.na(pedido)) pedido <- 50L
  pedido <- min(max(pedido, 1L), 100L)

  previos <- leer_previos()
  consulta <- CONSULTA_SB
  # pedir solo desde el último tuit capturado (ahorra créditos en duplicados)
  if (!is.null(previos) && nrow(previos) > 0) {
    desde <- as.Date(max(previos$fecha_hora, na.rm = TRUE))
    consulta <- paste(consulta, glue("since:{format(desde, '%Y-%m-%d')}"))
  }

  # La API pagina de a ~20 tuits por request (~21 créditos c/u): se recorren
  # páginas vía next_cursor hasta juntar `pedido` o agotar resultados.
  # El tier free además limita los requests por ventana (HTTP 429 = parar).
  crudos <- list()
  cursor <- NULL
  paginas_max <- ceiling(pedido / 20)
  for (pag in seq_len(paginas_max)) {
    req <- request("https://scrapebadger.com/v1/twitter/tweets/advanced_search") |>
      req_url_query(query = consulta, query_type = "Latest", count = pedido) |>
      req_headers(`x-api-key` = SB_KEY) |>
      req_timeout(60) |>
      req_error(is_error = function(r) FALSE)
    if (!is.null(cursor)) req <- req |> req_url_query(cursor = cursor)

    resp <- tryCatch(req_perform(req), error = function(e) {
      message("  error de red: ", conditionMessage(e)); NULL
    })
    if (is.null(resp)) break

    st <- resp_status(resp)
    if (st %in% c(401, 403)) { message("  HTTP ", st, ": key inválida — revisa SCRAPEBADGER_API_KEY"); break }
    if (st == 402) { message("  HTTP 402: créditos agotados en ScrapeBadger"); break }
    if (st == 429) { message("  HTTP 429: límite de requests del tier — se continúa con lo juntado"); break }
    if (st != 200) { message("  HTTP ", st, ": ", substr(resp_body_string(resp), 1, 300)); break }

    creditos <- resp_header(resp, "x-credits-used")
    body <- resp_body_json(resp)
    n_pag <- length(body$data %||% list())
    message(glue("  página {pag}: {n_pag} tuits ({creditos %||% '?'} créditos)"))
    if (n_pag == 0) break

    crudos <- c(crudos, body$data)
    cursor <- body$next_cursor
    if (is.null(cursor) || length(crudos) >= pedido) break
    pausar(1, 2)
  }

  if (length(crudos) == 0) {
    message("  0 tuits nuevos (desde la última captura)")
    return(invisible(NULL))
  }
  crudos <- head(crudos, pedido)

  tuits <- map(crudos, function(t) tibble(
    id           = as.character(t$id),
    fecha_hora   = as.POSIXct(t$created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"),
    texto        = limpiar_texto_poquito(t$full_text %||% t$text %||% ""),
    autor        = t$username %||% NA_character_,
    autor_nombre = t$user_name %||% t$username %||% NA_character_,
    seguidores   = as.integer(t$user_followers_count %||% NA),
    likes        = as.integer(t$favorite_count %||% 0),
    retweets     = as.integer(t$retweet_count %||% 0),
    respuestas   = as.integer(t$reply_count %||% 0),
    citas        = as.integer(t$quote_count %||% 0),
    impresiones  = suppressWarnings(as.integer(t$view_count %||% NA))
  )) |> list_rbind()

  tuits <- tuits |>
    filter(!is.na(id), nchar(texto) > 0) |>
    filtrar_chile() |>
    mutate(
      fecha = as.Date(with_tz(fecha_hora, "America/Santiago")),
      url = glue("https://x.com/{coalesce(autor, 'i')}/status/{id}"),
      fuente = "twitter",
      fecha_captura = now()
    )

  acumular_tuits(tuits, previos)
}

# —--------------------------------------------------------------------------
# Backend 2: API oficial de X v2 (search/recent) — respaldo
# —--------------------------------------------------------------------------
obtener_tweets_x <- function() {
  message("\n===== twitter (API oficial X v2) =====")

  cuota_mes <- suppressWarnings(as.integer(Sys.getenv("CGR_TW_CUOTA", "100")))
  pedido    <- suppressWarnings(as.integer(Sys.getenv("CGR_TW_MAX", "25")))
  if (is.na(cuota_mes)) cuota_mes <- 100L
  if (is.na(pedido)) pedido <- 25L

  previos <- leer_previos()

  # Presupuesto mensual: cada tuit guardado este mes ya gastó una lectura.
  # (since_id evita releer, así que guardados ≈ leídos.)
  usados_mes <- if (!is.null(previos) && "fecha_captura" %in% names(previos)) {
    sum(format(previos$fecha_captura, "%Y-%m") == format(now(), "%Y-%m"), na.rm = TRUE)
  } else 0L
  restante <- cuota_mes - usados_mes
  message(glue("  cuota mensual: {usados_mes}/{cuota_mes} usados ({restante} disponibles)"))
  if (restante < 10) {                       # la API exige max_results >= 10
    message("  cuota insuficiente para una request (mínimo 10) — se omite")
    return(invisible(NULL))
  }
  max_results <- min(max(pedido, 10L), restante, 100L)

  # since_id: id del tuit más reciente ya capturado, para pedir solo lo nuevo
  since_id <- NULL
  if (!is.null(previos) && nrow(previos) > 0) {
    since_id <- previos$id[which.max(previos$fecha_hora)]
  }

  req <- request("https://api.x.com/2/tweets/search/recent") |>
    req_url_query(
      query = CONSULTA_X,
      max_results = max_results,
      `tweet.fields` = "created_at,public_metrics,author_id,lang",
      expansions = "author_id",
      `user.fields` = "username,name,public_metrics"
    ) |>
    req_auth_bearer_token(TOKEN_X) |>
    req_timeout(30) |>
    req_error(is_error = function(r) FALSE)
  if (!is.null(since_id)) req <- req |> req_url_query(since_id = since_id)

  resp <- tryCatch(req_perform(req), error = function(e) {
    message("  error de red: ", conditionMessage(e)); NULL
  })
  if (is.null(resp)) return(invisible(NULL))

  st <- resp_status(resp)
  if (st == 401) { message("  HTTP 401: token inválido — revisa X_BEARER_TOKEN"); return(invisible(NULL)) }
  if (st == 429) { message("  HTTP 429: límite de la API (1 request/15 min o cuota mensual agotada)"); return(invisible(NULL)) }
  if (st != 200) { message("  HTTP ", st, ": ", substr(resp_body_string(resp), 1, 300)); return(invisible(NULL)) }

  body <- resp_body_json(resp)
  n_resp <- body$meta$result_count %||% 0L
  if (n_resp == 0 || is.null(body$data)) {
    message("  0 tuits nuevos (desde la última captura)")
    return(invisible(NULL))
  }

  usuarios <- map(body$includes$users, function(u) tibble(
    author_id    = u$id,
    autor        = u$username,
    autor_nombre = u$name %||% u$username,
    seguidores   = u$public_metrics$followers_count %||% NA_integer_
  )) |> list_rbind()

  tuits <- map(body$data, function(t) {
    m <- t$public_metrics
    tibble(
      id          = t$id,
      fecha_hora  = ymd_hms(t$created_at, tz = "UTC"),
      texto       = limpiar_texto_poquito(t$text),
      author_id   = t$author_id,
      likes       = m$like_count %||% 0L,
      retweets    = m$retweet_count %||% 0L,
      respuestas  = m$reply_count %||% 0L,
      citas       = m$quote_count %||% 0L,
      impresiones = m$impression_count %||% NA_integer_
    )
  }) |> list_rbind()

  tuits <- tuits |>
    left_join(usuarios, by = "author_id") |>
    select(-author_id) |>
    filtrar_chile() |>
    mutate(
      fecha = as.Date(with_tz(fecha_hora, "America/Santiago")),
      url = glue("https://x.com/{coalesce(autor, 'i')}/status/{id}"),
      fuente = "twitter",
      fecha_captura = now()
    )

  acumular_tuits(tuits, previos)
}

# —--------------------------------------------------------------------------
# Selección de backend
# —--------------------------------------------------------------------------
if (nzchar(SB_KEY)) {
  obtener_tweets_scrapebadger()
} else if (nzchar(TOKEN_X)) {
  obtener_tweets_x()
} else {
  message("\n===== twitter =====")
  message("  sin credenciales: define SCRAPEBADGER_API_KEY (o X_BEARER_TOKEN) — se omite")
}
