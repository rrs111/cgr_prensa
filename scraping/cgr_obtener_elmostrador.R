# CGR Prensa — scraping de El Mostrador (elmostrador.cl)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: .d-the-single-wrapper__text
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "elmostrador",
  secciones_reciente = c(
    "https://www.elmostrador.cl/noticias/pais/",
    "https://www.elmostrador.cl/mercados/"
  ),
  patron        = "elmostrador\\.cl/.+/\\d{4}/\\d{2}/\\d{2}/[a-z0-9-]+/",
  sel_cuerpo    = ".d-the-single-wrapper__text",
  base          = "https://www.elmostrador.cl",
  plantilla_hist = "https://www.elmostrador.cl/noticias/pais/page/{n}/",
  plantilla_busqueda = "https://www.elmostrador.cl/?s={q}"
)
