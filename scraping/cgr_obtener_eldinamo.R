# CGR Prensa — scraping de El Dínamo (eldinamo.cl)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: .the-article__body
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "eldinamo",
  secciones_reciente = c(
    "https://www.eldinamo.cl/pais/",
    "https://www.eldinamo.cl/dinero/"
  ),
  patron     = "eldinamo\\.cl/[a-z-]+/\\d{4}/\\d{2}/\\d{2}/[a-z0-9-]+/",
  sel_cuerpo = ".the-article__body",
  base       = "https://www.eldinamo.cl",
  plantilla_busqueda = "https://www.eldinamo.cl/?s={q}"
)
