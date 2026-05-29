# CGR Prensa — scraping de Pauta (pauta.cl)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: .d-the-article__text
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "pauta",
  secciones_reciente = c(
    "https://www.pauta.cl/actualidad/",
    "https://www.pauta.cl/economia/"
  ),
  patron     = "pauta\\.cl/[a-z-]+/\\d{4}/\\d{2}/\\d{2}/[a-z0-9-]+\\.html$",
  sel_cuerpo = ".d-the-article__text",
  base       = "https://www.pauta.cl",
  plantilla_busqueda = "https://www.pauta.cl/?s={q}"
)
