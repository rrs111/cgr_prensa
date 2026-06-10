# CGR Prensa — scraping de Cooperativa (cooperativa.cl)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: #cuerpo-ad
# Fecha desde la URL (.../YYYY-MM-DD/HHMMSS.html).
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "cooperativa",
  secciones_reciente = c(
    "https://www.cooperativa.cl/noticias/pais",
    "https://www.cooperativa.cl/noticias/economia"
  ),
  patron     = "/noticias/[a-z-]+/.+/\\d{4}-\\d{2}-\\d{2}/\\d+\\.html$",
  sel_cuerpo = "#cuerpo-ad",
  bing_dominio = "cooperativa.cl",
  base       = "https://www.cooperativa.cl"
)
