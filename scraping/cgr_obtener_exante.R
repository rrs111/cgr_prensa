# CGR Prensa — scraping de Ex-Ante (ex-ante.cl)
# BLOQUEADO POR CLOUDFLARE: verificado 2026-05-28 que incluso con Chrome headless
# (chromote) la respuesta es la página de desafío de Cloudflare
# ("Attention Required! | Cloudflare"), no la noticia. No es scrapeable sin un
# servicio de bypass anti-bot (fuera de alcance / contra los términos del sitio).
# El módulo se deja por completitud; normalmente devolverá 0 noticias.
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "exante",
  secciones_reciente = c(
    "https://www.ex-ante.cl/"
  ),
  patron     = "ex-ante\\.cl/[a-z0-9-]{15,}/?$",
  sel_cuerpo = ".single-content",
  base       = "https://www.ex-ante.cl",
  plantilla_busqueda = "https://www.ex-ante.cl/?s={q}",
  js         = TRUE
)
