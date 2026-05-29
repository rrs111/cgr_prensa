# CGR Prensa — scraping de Interferencia (interferencia.cl)
# Verificado en vivo 2026-05-28 con rvest (Drupal). Cuerpo: .field-item
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "interferencia",
  secciones_reciente = c(
    "https://interferencia.cl/",
    "https://interferencia.cl/secciones/politica",
    "https://interferencia.cl/secciones/empresas"
  ),
  patron     = "/articulos/[a-z0-9-]{10,}$",
  sel_cuerpo = ".field-item",
  base       = "https://interferencia.cl",
  plantilla_busqueda = "https://interferencia.cl/search/node/{q}"
)
