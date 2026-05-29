# CGR Prensa — scraping de La Segunda (lasegunda.com)
# Verificado en vivo 2026-05-28 con rvest. El CUERPO está tras paywall:
# solo se obtienen título (og:title) + bajada (meta description) + fecha.
# Igual aporta señal de cobertura/titulares sobre la CGR.
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "lasegunda",
  secciones_reciente = c(
    "https://www.lasegunda.com/",
    "https://www.lasegunda.com/Politica",
    "https://www.lasegunda.com/Economia",
    "https://www.lasegunda.com/Internacional"
  ),
  patron     = "/Noticias/[A-Za-z-]+/\\d{4}/\\d{2}/\\d+/",
  sel_cuerpo = "article",   # paywall: normalmente vacío; la señal va en la bajada
  base       = "https://www.lasegunda.com"
)
