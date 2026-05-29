# CGR Prensa — scraping de La Tercera (latercera.com)
# El LISTADO de secciones se lee con rvest, pero las NOTICIAS se renderizan con
# JavaScript (Arc Publishing), por lo que requiere chromote + Chrome instalado.
# Verificado 2026-05-28: el listado entrega enlaces /<seccion>/noticia/<slug>/.
# Selector de cuerpo (.single-content) sujeto a confirmación con Chrome disponible.
# Si no hay Chrome, el módulo no devuelve datos (se registra en el log).
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "latercera",
  secciones_reciente = c(
    "https://www.latercera.com/categoria/nacional/",
    "https://www.latercera.com/categoria/politica/",
    "https://www.latercera.com/categoria/negocios/"
  ),
  patron         = "/[a-z0-9-]+/noticia/[a-z0-9-]+",
  sel_cuerpo     = "[class*=article-body]",
  base           = "https://www.latercera.com",
  plantilla_hist = "https://www.latercera.com/categoria/nacional/page/{n}/",
  plantilla_busqueda = "https://www.latercera.com/?s={q}",
  js             = TRUE
)
