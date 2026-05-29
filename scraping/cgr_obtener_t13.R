# CGR Prensa — scraping de T13 (t13.cl)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: .cuerpo-content
# Fecha desde el slug (.../slug-D-M-YYYY) y meta.
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "t13",
  secciones_reciente = c(
    "https://www.t13.cl/",
    "https://www.t13.cl/noticias/politica",
    "https://www.t13.cl/noticias/negocios"
  ),
  patron     = "/noticia/[a-z]",
  sel_cuerpo = ".cuerpo-content",
  base       = "https://www.t13.cl"
)
