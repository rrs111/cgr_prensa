# CGR Prensa — scraping de CIPER Chile (ciperchile.cl)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: main .col-lg-9
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "ciper",
  secciones_reciente = c(
    "https://www.ciperchile.cl/category/actualidad/",
    "https://www.ciperchile.cl/category/investigacion/"
  ),
  patron            = "ciperchile\\.cl/\\d{4}/\\d{2}/\\d{2}/",
  sel_cuerpo        = "main .col-lg-9",
  base              = "https://www.ciperchile.cl",
  plantilla_hist    = "https://www.ciperchile.cl/category/actualidad/page/{n}/",
  plantilla_busqueda = "https://www.ciperchile.cl/?s={q}"
)
