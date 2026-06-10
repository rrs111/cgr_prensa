# Motor de scraping compartido por todos los módulos cgr_obtener_*.R
# Estrategia: descarga educada con {polite} (respeta robots.txt) y fallback a
# {httr2}. Los selectores de cuerpo de cada medio fueron verificados en vivo.
# Para medios con render JavaScript se usa {chromote} (requiere Chrome instalado).

suppressPackageStartupMessages({
  library(rvest)
  library(polite)
  library(httr2)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(glue)
  library(lubridate)
})

# términos de búsqueda CGR (terminos_busqueda)
if (file.exists("datos/cgr_terminos.R")) source("datos/cgr_terminos.R")
if (!exists("terminos_busqueda")) terminos_busqueda <- c("contraloria", "contralor", "probidad")

# Identificador honesto del bot
UA_CGR <- paste("CGR-Prensa (monitor academico de prensa sobre la Contraloria);",
                "+https://github.com/; uso no comercial")

# —--------------------------------------------------------------------------
# Descarga
# —--------------------------------------------------------------------------

# Descarga educada: intenta polite (robots.txt), si falla usa httr2 directo.
descargar <- function(url) {
  res <- tryCatch(
    suppressWarnings(polite::bow(url, user_agent = UA_CGR, delay = 1) |> polite::scrape()),
    error = function(e) NULL
  )
  if (is.null(res)) res <- leer_html(url)
  res
}

# ¿Hay Chrome disponible para chromote?
tiene_chrome <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) return(FALSE)
  !is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))
}

# Asegura argumentos de Chrome compatibles con entornos restringidos / CI
# (sin --no-sandbox, Chrome no abre el puerto de depuración como root o en sandbox).
.chrome_args_ok <- FALSE
asegurar_chrome_args <- function() {
  if (.chrome_args_ok) return(invisible())
  extra <- c("--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage")
  actuales <- tryCatch(chromote::get_chrome_args(), error = function(e) character(0))
  faltan <- setdiff(extra, actuales)
  if (length(faltan)) try(chromote::set_chrome_args(c(actuales, faltan)), silent = TRUE)
  options(chromote.timeout = 60)
  .chrome_args_ok <<- TRUE
  invisible()
}

# Descarga renderizando JavaScript con Chrome headless (chromote)
descargar_js <- function(url, espera = 3) {
  if (!tiene_chrome()) {
    message("  (chromote/Chrome no disponible — se omite ", url, ")")
    return(NULL)
  }
  asegurar_chrome_args()
  tryCatch({
    b <- chromote::ChromoteSession$new()
    on.exit(try(b$close(), silent = TRUE), add = TRUE)
    b$Page$navigate(url, wait_ = TRUE)
    Sys.sleep(espera)
    html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
    rvest::read_html(html)
  }, error = function(e) {
    message("  error chromote: ", conditionMessage(e)); NULL
  })
}

# Selector de descargador según motor
.obtener_html <- function(url, js = FALSE) if (js) descargar_js(url) else descargar(url)

# —--------------------------------------------------------------------------
# Extracción
# —--------------------------------------------------------------------------

# Enlaces de noticias desde una página de sección
obtener_enlaces <- function(url_seccion, patron, base = "", js = FALSE) {
  h <- .obtener_html(url_seccion, js)
  if (is.null(h)) return(character(0))
  hrefs <- h |> html_elements("a") |> html_attr("href")
  hrefs <- unique(hrefs[!is.na(hrefs)])
  # quitar ancla y query ANTES de filtrar (descarta enlaces de compartir en redes)
  hrefs <- hrefs |> str_remove("#.*$") |> str_remove("\\?.*$")
  hrefs <- hrefs[str_detect(hrefs, patron)]
  hrefs <- ifelse(str_detect(hrefs, "^https?://"), hrefs, paste0(base, hrefs))
  unique(hrefs)
}

# Fecha de publicación: meta -> URL -> <time> -> NA (p1 completará con fecha_scraping)
extraer_fecha <- function(h, url) {
  f <- h |> html_element('meta[property="article:published_time"]') |> html_attr("content")
  if (length(f) && !is.na(f) && nchar(f) >= 8) return(str_sub(f, 1, 10))

  f2 <- str_extract(url, "\\d{4}/\\d{2}/\\d{2}")
  if (!is.na(f2)) return(str_replace_all(f2, "/", "-"))

  f3 <- str_extract(url, "\\d{4}-\\d{2}-\\d{2}")
  if (!is.na(f3)) return(f3)

  f4 <- str_extract(url, "/\\d{8}/")            # df: /20250528/
  if (!is.na(f4)) {
    d <- str_extract(f4, "\\d{8}")
    return(paste(str_sub(d, 1, 4), str_sub(d, 5, 6), str_sub(d, 7, 8), sep = "-"))
  }
  f5 <- str_extract(url, "\\d{1,2}-\\d{1,2}-\\d{4}$")  # t13: -28-5-2026
  if (!is.na(f5)) return(format(dmy(f5), "%Y-%m-%d"))

  ft <- h |> html_element("time") |> html_attr("datetime")
  if (length(ft) && !is.na(ft) && nchar(ft) >= 8) return(str_sub(ft, 1, 10))

  NA_character_
}

# Limpia sufijos de marca del título
limpiar_titulo <- function(x) {
  x |>
    str_remove("\\s*[-|–]\\s*(La Segunda\\.com|Emol|Diario Financiero|DF\\.cl|BioBioChile|T13|CNN Chile|El Mostrador|Pauta|El L[íi]bero).*$") |>
    str_squish()
}

# Scrapea UN artículo con el patrón común (meta + selector de cuerpo verificado)
scrapear_articulo <- function(url, fuente, sel_cuerpo, js = FALSE) {
  if (!js && is.null(revisar_url(url))) return(NULL)
  tryCatch({
    h <- .obtener_html(url, js)
    if (is.null(h)) return(NULL)

    titulo <- h |> html_element('meta[property="og:title"]') |> html_attr("content")
    if (is.na(titulo)) titulo <- h |> html_element("h1") |> html_text2()
    titulo <- limpiar_titulo(validar_elementos(titulo))

    bajada <- h |> html_element('meta[name="description"]') |> html_attr("content")
    bajada <- limpiar_texto_poquito(validar_elementos(bajada))

    cuerpo_els <- h |> html_elements(paste0(sel_cuerpo, " p"))
    if (length(cuerpo_els) == 0) cuerpo_els <- h |> html_elements(sel_cuerpo)  # ej. Emol .EmolText
    cuerpo <- cuerpo_els |> html_text2() |> limpiar_texto_poquito()
    cuerpo <- cuerpo[nchar(cuerpo) > 30]
    cuerpo <- validar_elementos(cuerpo, colapsar = TRUE)

    tibble(
      titulo = titulo,
      bajada = bajada,
      cuerpo = cuerpo,
      fecha = extraer_fecha(h, url),
      fuente = fuente,
      url = url,
      fecha_scraping = now()
    )
  }, error = function(e) {
    message("  error artículo ", fuente, ": ", conditionMessage(e)); NULL
  })
}

# —--------------------------------------------------------------------------
# Orquestación por fuente
# —--------------------------------------------------------------------------

# Recorre secciones -> reúne enlaces -> scrapea cada noticia -> guarda .rds
#
# fuente        nombre corto (slug) de la fuente
# secciones     vector de URLs de sección/portada
# patron        regex para reconocer URLs de noticias
# sel_cuerpo    selector CSS del contenedor del cuerpo (verificado en vivo)
# base          dominio base para completar URLs relativas
# hist          sufijo "_hist" cuando es scraping histórico
# max_articulos límite de noticias por corrida (NULL = sin límite)
# js            TRUE para renderizar con chromote (medios con JavaScript)
scrapear_fuente <- function(fuente, secciones, patron, sel_cuerpo,
                            base = "", hist = "", max_articulos = NULL,
                            js = FALSE, pausa = c(1, 3)) {
  message(glue("\n===== {fuente} ====="))

  enlaces <- map(secciones, function(s) {
    message(glue("  sección: {s}"))
    e <- obtener_enlaces(s, patron, base, js)
    pausar(pausa[1], pausa[2])
    e
  }) |> unlist() |> unique()

  if (!continuar_si_hay_enlaces(enlaces)) return(invisible(NULL))

  if (!is.null(max_articulos) && is.finite(max_articulos)) {
    enlaces <- head(enlaces, max_articulos)
  }

  datos <- map(enlaces, function(u) {
    r <- scrapear_articulo(u, fuente, sel_cuerpo, js)
    pausar(pausa[1], pausa[2])
    r
  }) |> list_rbind()

  revisar_scraping(datos, fuente)
  if (!is.null(datos) && nrow(datos) > 0) {
    saveRDS(datos, ruta_resultado(fuente, hist))
  }
  invisible(datos)
}

# Genera páginas paginadas para modo histórico (ej. .../page/2, .../page/3 ...)
paginar <- function(plantilla, desde = 1, hasta = 5) {
  as.character(glue(plantilla, n = desde:hasta))
}

# Scraping POR BÚSQUEDA: consulta los términos CGR en el buscador del medio.
# Es la vía más eficiente para un monitor temático (mucha más cobertura CGR
# que scrapear secciones generales y filtrar). `plantilla_busqueda` usa {q}
# (y opcionalmente {n} para paginar resultados).
buscar_fuente <- function(fuente, plantilla_busqueda, terminos, patron, sel_cuerpo,
                          base = "", js = FALSE, max_articulos = NULL, pausa = c(1, 3)) {
  message(glue("\n===== {fuente} (búsqueda) ====="))

  enlaces <- map(terminos, function(t) {
    q <- str_replace_all(str_trim(t), "\\s+", "+")
    url <- as.character(glue(plantilla_busqueda, q = q, n = 1))
    message(glue("  buscar '{t}'"))
    e <- obtener_enlaces(url, patron, base, js)
    pausar(pausa[1], pausa[2])
    e
  }) |> unlist() |> unique()

  if (!continuar_si_hay_enlaces(enlaces)) return(invisible(NULL))
  if (!is.null(max_articulos) && is.finite(max_articulos)) enlaces <- head(enlaces, max_articulos)

  datos <- map(enlaces, function(u) {
    r <- scrapear_articulo(u, fuente, sel_cuerpo, js)
    pausar(pausa[1], pausa[2])
    r
  }) |> list_rbind()

  revisar_scraping(datos, fuente)
  if (!is.null(datos) && nrow(datos) > 0) saveRDS(datos, ruta_resultado(fuente, "_busqueda"))
  invisible(datos)
}

# —--------------------------------------------------------------------------
# Google News RSS: vía de acceso para medios con paywall o Cloudflare
# —--------------------------------------------------------------------------
# Para medios que no se pueden scrapear directo (La Segunda, Diario Financiero,
# The Clinic, Ex-Ante), Google News RSS entrega titular + fecha + enlace sin
# bloqueo. No hay bajada ni cuerpo, pero el titular basta para el filtro de
# relevancia CGR (que usa título+bajada) y para el monitoreo de cobertura.
# El enlace es el redirect de Google (estable por artículo: sirve para
# deduplicar y, al hacer clic, lleva a la nota original).
buscar_google_news <- function(fuente, consulta, max_items = 100) {
  message(glue("\n===== {fuente} (Google News RSS) ====="))
  url <- paste0(
    "https://news.google.com/rss/search?q=", utils::URLencode(consulta, reserved = TRUE),
    "&hl=es-419&gl=CL&ceid=CL:es-419"
  )
  feed <- tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_user_agent(UA_CGR) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    xml2::read_xml(httr2::resp_body_raw(resp))
  }, error = function(e) {
    message("  error Google News (", fuente, "): ", conditionMessage(e)); NULL
  })
  if (is.null(feed)) return(invisible(NULL))

  items <- xml2::xml_find_all(feed, ".//item")
  if (length(items) == 0) { message("  0 ítems"); return(invisible(NULL)) }
  items <- head(items, max_items)

  titulo <- xml2::xml_text(xml2::xml_find_first(items, "./title"))
  enlace <- xml2::xml_text(xml2::xml_find_first(items, "./link"))
  pubdate <- xml2::xml_text(xml2::xml_find_first(items, "./pubDate"))
  medio <- xml2::xml_text(xml2::xml_find_first(items, "./source"))

  # quitar el sufijo " - Medio" que Google añade al titular
  titulo <- str_remove(titulo, "\\s+[-–]\\s+[^-–]{2,60}$") |> str_squish()

  fecha <- suppressWarnings(
    lubridate::parse_date_time(pubdate, orders = "a, d b Y H:M:S", locale = "C")
  )

  datos <- tibble(
    titulo = titulo,
    bajada = NA_character_,
    cuerpo = NA_character_,
    fecha = format(as.Date(fecha), "%Y-%m-%d"),
    fuente = fuente,
    url = enlace,
    fecha_scraping = now()
  ) |>
    filter(!is.na(titulo), nchar(titulo) > 15, !is.na(url)) |>
    distinct(url, .keep_all = TRUE)

  revisar_scraping(datos, fuente)
  if (nrow(datos) > 0) saveRDS(datos, ruta_resultado(fuente, "_gnews"))
  invisible(datos)
}

# —--------------------------------------------------------------------------
# Bing News RSS: búsqueda para medios SIN buscador propio en su sitio
# —--------------------------------------------------------------------------
# A diferencia de Google News, el RSS de Bing trae la URL real del artículo
# (en el parámetro url= de su redirect apiclick.aspx), así que sirve como
# DESCUBRIMIENTO y luego cada nota se scrapea completa del sitio del medio
# (con el mismo selector de cuerpo del módulo). Ideal para Emol, Cooperativa,
# T13 o El Líbero, que no tienen búsqueda por URL.
buscar_bing_news <- function(fuente, dominio, patron, sel_cuerpo, base = "",
                             js = FALSE, max_articulos = NULL, pausa = c(1, 3),
                             paginas = 2) {
  message(glue("\n===== {fuente} (Bing News RSS) ====="))
  q <- utils::URLencode(
    glue('(contraloria OR contralor OR contralora OR "dorothy perez") site:{dominio}'),
    reserved = TRUE
  )
  enlaces <- map(seq_len(paginas), function(p) {
    url <- glue("https://www.bing.com/news/search?q={q}&format=rss&first={(p - 1) * 12 + 1}")
    feed <- tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_user_agent(UA_CGR) |>
        httr2::req_timeout(30) |>
        httr2::req_perform()
      xml2::read_xml(httr2::resp_body_raw(resp))
    }, error = function(e) {
      message("  error Bing News (", fuente, "): ", conditionMessage(e)); NULL
    })
    if (is.null(feed)) return(character(0))
    links <- xml2::xml_text(xml2::xml_find_all(feed, ".//item/link"))
    # URL real desde el redirect de Bing (param url=); si no hay, el link tal cual
    crudos <- str_match(links, "[?&]url=([^&]+)")[, 2]
    reales <- ifelse(is.na(crudos), links,
                     vapply(crudos, function(x) utils::URLdecode(x), character(1)))
    pausar(pausa[1], pausa[2])
    reales
  }) |> unlist() |> unique()

  enlaces <- enlaces[!is.na(enlaces) & str_detect(enlaces, patron)]
  if (!continuar_si_hay_enlaces(enlaces)) return(invisible(NULL))
  if (!is.null(max_articulos) && is.finite(max_articulos)) enlaces <- head(enlaces, max_articulos)

  datos <- map(enlaces, function(u) {
    r <- scrapear_articulo(u, fuente, sel_cuerpo, js)
    pausar(pausa[1], pausa[2])
    r
  }) |> list_rbind()

  revisar_scraping(datos, fuente)
  if (!is.null(datos) && nrow(datos) > 0) saveRDS(datos, ruta_resultado(fuente, "_bing"))
  invisible(datos)
}

# Punto de entrada estándar de cada módulo. Controla el modo mediante variables
# de entorno (para que el orquestador o el usuario lo ajuste sin editar módulos):
#   CGR_MODO   "completo" (def.) = búsqueda CGR + secciones recientes
#              "buscar"   = solo búsqueda de términos CGR en el medio
#              "reciente" = solo secciones recientes
#              "hist"     = secciones con paginación hacia atrás
#   CGR_HIST_PAGINAS  nº de páginas a paginar en histórico (def. 5)
#   CGR_MAX           límite de noticias por corrida (def. sin límite)
#   CGR_PAUSA_MIN/MAX pausa entre requests en segundos (acelera pruebas)
ejecutar_modulo <- function(fuente, secciones_reciente, patron, sel_cuerpo,
                            base = "", plantilla_hist = NULL, plantilla_busqueda = NULL,
                            bing_dominio = NULL, js = FALSE, pausa = c(1, 3)) {
  modo    <- Sys.getenv("CGR_MODO", "completo")
  paginas <- suppressWarnings(as.integer(Sys.getenv("CGR_HIST_PAGINAS", "5")))
  max_art <- suppressWarnings(as.integer(Sys.getenv("CGR_MAX", "")))
  if (is.na(max_art)) max_art <- NULL
  if (is.na(paginas)) paginas <- 5

  pmin <- suppressWarnings(as.numeric(Sys.getenv("CGR_PAUSA_MIN", "")))
  pmax <- suppressWarnings(as.numeric(Sys.getenv("CGR_PAUSA_MAX", "")))
  if (!is.na(pmin) && !is.na(pmax)) pausa <- c(pmin, pmax)

  secciones_o_articulo <- function(secciones, hist) {
    scrapear_fuente(fuente, secciones, patron, sel_cuerpo, base = base,
                    hist = hist, max_articulos = max_art, js = js, pausa = pausa)
  }

  # 1) Búsqueda (si el medio la soporta y el modo lo pide)
  if (modo %in% c("buscar", "completo") && !is.null(plantilla_busqueda)) {
    buscar_fuente(fuente, plantilla_busqueda, terminos_busqueda, patron, sel_cuerpo,
                  base = base, js = js, max_articulos = max_art, pausa = pausa)
  }

  # 1b) Búsqueda vía Bing News RSS (medios sin buscador propio en su sitio)
  if (modo %in% c("buscar", "completo") && !is.null(bing_dominio)) {
    buscar_bing_news(fuente, bing_dominio, patron, sel_cuerpo, base = base,
                     js = js, max_articulos = max_art, pausa = pausa)
  }

  # 2) Secciones recientes (modos completo/reciente, o buscar sin búsqueda alguna)
  if (modo %in% c("reciente", "completo") ||
      (modo == "buscar" && is.null(plantilla_busqueda) && is.null(bing_dominio))) {
    secciones_o_articulo(secciones_reciente, "")
  }

  # 3) Histórico con paginación
  if (modo == "hist") {
    secciones <- if (!is.null(plantilla_hist)) paginar(plantilla_hist, 1, paginas) else secciones_reciente
    secciones_o_articulo(secciones, "_hist")
  }

  invisible(NULL)
}
