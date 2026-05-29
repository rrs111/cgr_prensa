# CGR Prensa — scraping de El Desconcierto (eldesconcierto.cl)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: .article-body
# Las URLs terminan en -nNNNNN; la fecha se toma de meta/<time>.
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "eldesconcierto",
  secciones_reciente = c(
    "https://eldesconcierto.cl/",
    "https://eldesconcierto.cl/nacional",
    "https://eldesconcierto.cl/economia"
  ),
  patron     = "eldesconcierto\\.cl/[a-z-]+/.+-n\\d+$",
  sel_cuerpo = ".article-body",
  base       = "https://eldesconcierto.cl",
  plantilla_busqueda = "https://eldesconcierto.cl/?s={q}"
)
