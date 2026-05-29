# Funciones auxiliares compartidas por el scraping y el procesamiento.
# Compatible tanto con RStudio como con `Rscript` (no usa callr ni rstudioapi).

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(glue)
  library(lubridate)
})

# —--------------------------------------------------------------------------
# Scraping: red y rutas
# —--------------------------------------------------------------------------

# Pausa aleatoria entre requests para ser respetuoso con los servidores
pausar <- function(min = 1, max = 3) {
  Sys.sleep(runif(1, min, max))
}

# Número aleatorio para nombres de archivo
rng <- function() sample(1111:9999, 1)

# Ruta donde se guarda el resultado de un módulo de scraping
ruta_resultado <- function(fuente, hist = "", formato = "rds") {
  if (!is.character(hist)) hist <- ""
  dir <- glue("scraping/datos/{fuente}")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  glue("{dir}/{fuente}_{rng()}_{lubridate::today()}{hist}.{formato}")
}

# Revisa que una URL responda 200 antes de scrapearla. Devuelve NULL si falla.
revisar_url <- function(url) {
  estado <- tryCatch(
    httr::status_code(httr::GET(url, httr::timeout(20))),
    error = function(e) NULL
  )
  if (is.null(estado) || !is.numeric(estado)) return(NULL)
  if (estado != 200) {
    message(glue("  ! HTTP {estado} en {url}"))
    return(NULL)
  }
  estado
}

# Lectura de HTML robusta con user-agent realista (sin polite).
# polite::bow()|>scrape() respeta robots.txt; esta es la alternativa httr2 directa.
leer_html <- function(url, intentos = 2) {
  ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
  for (i in seq_len(intentos)) {
    resultado <- tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_user_agent(ua) |>
        httr2::req_timeout(30) |>
        httr2::req_retry(max_tries = 1) |>
        httr2::req_perform()
      rvest::read_html(httr2::resp_body_string(resp))
    }, error = function(e) NULL)
    if (!is.null(resultado)) return(resultado)
    pausar(1, 2)
  }
  NULL
}

# Continúa solo si se obtuvo una cantidad mínima de enlaces
continuar_si_hay_enlaces <- function(enlaces, minimo = 1) {
  if (length(enlaces) < minimo) {
    message(glue("  enlaces insuficientes ({length(enlaces)}), terminando"))
    return(FALSE)
  }
  message(glue("  {length(enlaces)} enlaces obtenidos"))
  TRUE
}

# —--------------------------------------------------------------------------
# Limpieza de elementos y texto
# —--------------------------------------------------------------------------

# Cambia elementos vacíos por NA; opcionalmente colapsa vectores en un string
validar_elementos <- function(input, colapsar = FALSE) {
  if (length(input) == 0) return(NA_character_)
  if (colapsar) input <- paste(input, collapse = "\n")
  input
}

# Limpieza ligera: saltos de línea y espacios
limpiar_texto_poquito <- function(x) {
  x |>
    str_replace_all("\\n|\\r|\\t", " ") |>
    str_squish() |>
    str_trim()
}

# Limpieza profunda para tokenización (elimina código, puntuación, dígitos)
limpiar_texto <- function(x) {
  x |>
    str_replace_all("\\{.*?\\}", " ") |>
    str_replace_all("\\#\\w+", " ") |>
    str_remove_all("\\{\\{.*?\\}\\}") |>
    str_replace_all("[[:punct:]]", " ") |>
    str_replace_all("[[:digit:]]", " ") |>
    str_replace_all("\\||<|>|@|-|—|“|”|»|«", " ") |>
    str_squish() |>
    str_trim()
}

# —--------------------------------------------------------------------------
# Fuentes y fechas
# —--------------------------------------------------------------------------

# Nombre legible de cada fuente para mostrar en la app.
# Los nombres se alinean con los del proyecto prensa_chile (y su muestra de
# 10.000 noticias) para que las fuentes scrapeadas por nosotros y las importadas
# de la muestra se agrupen como una sola.
recodificar_fuentes <- function(data) {
  data |>
    mutate(fuente = {
      f <- as.character(fuente)
      out <- case_match(
        f,
        "emol"           ~ "Emol",
        "latercera"      ~ "La Tercera",
        "lasegunda"      ~ "La Segunda",
        "biobio"         ~ "Radio BíoBío",
        "ciper"          ~ "Ciper",
        "elmostrador"    ~ "El Mostrador",
        "cooperativa"    ~ "Cooperativa",
        "df"             ~ "D. Financiero",
        "exante"         ~ "Ex-Ante",
        "cnnchile"       ~ "CNN Chile",
        "t13"            ~ "T13",
        "pauta"          ~ "Pauta",
        "eldinamo"       ~ "El Dínamo",
        "eldesconcierto" ~ "El Desconcierto",
        "interferencia"  ~ "Interferencia",
        "ellibero"       ~ "El Líbero",
        "theclinic"      ~ "The Clinic",
        .default = f
      )
      # Marcar como UTF-8 para que coincida con los datos externos (la muestra)
      # aun bajo un locale C, y unificar variantes de nombre.
      Encoding(out) <- "UTF-8"
      out <- case_match(out,
        "BioBioChile"       ~ "Radio BíoBío",
        "Diario Financiero" ~ "D. Financiero",
        "CIPER"             ~ "Ciper",
        .default = out)
      Encoding(out) <- "UTF-8"
      out
    })
}

mes_a_numero <- function(x) {
  x <- tolower(str_trim(x))
  recode(x,
         "enero" = "1", "febrero" = "2", "marzo" = "3", "abril" = "4",
         "mayo" = "5", "junio" = "6", "julio" = "7", "agosto" = "8",
         "septiembre" = "9", "setiembre" = "9", "octubre" = "10",
         "noviembre" = "11", "diciembre" = "12", .default = NA_character_)
}

redactar_fecha <- function(x) {
  meses <- c("enero","febrero","marzo","abril","mayo","junio",
             "julio","agosto","septiembre","octubre","noviembre","diciembre")
  paste(lubridate::day(x), "de", meses[lubridate::month(x)])
}

# —--------------------------------------------------------------------------
# Reporte
# —--------------------------------------------------------------------------

# Mensaje resumen tras un scraping
revisar_scraping <- function(data, fuente = "") {
  n <- if (is.data.frame(data)) nrow(data) else 0
  message(glue("listo {fuente} — {n} noticias — {format(now(), '%Y-%m-%d %H:%M')}"))
  invisible(data)
}

# Notificación (en local usa osascript en macOS; en CI solo imprime)
notificacion <- function(titulo = "CGR Prensa", texto = "") {
  message(glue("{titulo}: {texto}"))
  if (Sys.info()[["sysname"]] == "Darwin" && interactive()) {
    try(system(glue("osascript -e 'display notification \"{texto}\" with title \"{titulo}\"'")),
        silent = TRUE)
  }
}
