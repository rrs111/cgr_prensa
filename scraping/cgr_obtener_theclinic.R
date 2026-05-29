# CGR Prensa — scraping de The Clinic (theclinic.cl)
# BLOQUEADO POR CLOUDFLARE: las noticias responden 403 a peticiones directas y,
# con Chrome headless (chromote), devuelven la página de desafío de Cloudflare
# ("Un momento…"). Verificado 2026-05-28. No es scrapeable sin un servicio de
# bypass anti-bot (fuera de alcance). El módulo se deja por completitud;
# normalmente devolverá 0 noticias.
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "theclinic",
  secciones_reciente = c(
    "https://www.theclinic.cl/",
    "https://www.theclinic.cl/categoria/actualidad/"
  ),
  patron     = "theclinic\\.cl/\\d{4}/\\d{2}/\\d{2}/[a-z0-9-]+/",
  sel_cuerpo = ".entry-content",
  base       = "https://www.theclinic.cl",
  plantilla_busqueda = "https://www.theclinic.cl/?s={q}",
  js         = TRUE
)
