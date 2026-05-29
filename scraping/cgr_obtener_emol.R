# CGR Prensa — scraping de Emol (emol.com)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: .EmolText
# Fecha desde meta article:published_time y URL (/YYYY/MM/DD/).
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "emol",
  secciones_reciente = c(
    "https://www.emol.com/nacional/",
    "https://www.emol.com/economia/"
  ),
  patron     = "/noticias/[A-Za-z]+/\\d{4}/\\d{2}/\\d{2}/",
  sel_cuerpo = ".EmolText",
  base       = "https://www.emol.com"
)
