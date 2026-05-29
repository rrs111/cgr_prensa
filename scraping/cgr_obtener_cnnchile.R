# CGR Prensa — scraping de CNN Chile (cnnchile.com)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: .main-article__text
# Las URLs no traen fecha; se toma de meta/<time> y, si falta, p1 usa fecha_scraping.
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "cnnchile",
  secciones_reciente = c(
    "https://www.cnnchile.com/pais/",
    "https://www.cnnchile.com/negocios/"
  ),
  patron     = "cnnchile\\.com/[a-z]+/[a-z0-9-]{20,}/?$",
  sel_cuerpo = ".main-article__text",
  base       = "https://www.cnnchile.com",
  plantilla_busqueda = "https://www.cnnchile.com/?s={q}"
)
