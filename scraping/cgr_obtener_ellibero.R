# CGR Prensa — scraping de El Líbero (ellibero.cl)
# Verificado en vivo 2026-05-28 con rvest (WordPress). Cuerpo: .entry-content
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "ellibero",
  secciones_reciente = c(
    "https://ellibero.cl/actualidad/",
    "https://ellibero.cl/investigacion/"
  ),
  patron         = "ellibero\\.cl/[a-z-]+/[a-z0-9-]{20,}/?$",
  sel_cuerpo     = ".entry-content",
  base           = "https://ellibero.cl",
  plantilla_hist = "https://ellibero.cl/actualidad/page/{n}/"
)
