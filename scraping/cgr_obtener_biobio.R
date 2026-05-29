# CGR Prensa — scraping de BioBioChile (biobiochile.cl)
# Verificado en vivo 2026-05-28 con rvest. Cuerpo: .post-content
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "biobio",
  secciones_reciente = c(
    "https://www.biobiochile.cl/lista/categorias/nacional",
    "https://www.biobiochile.cl/lista/categorias/economia"
  ),
  patron     = "/noticias/.+/\\d{4}/\\d{2}/\\d{2}/.+\\.shtml",
  sel_cuerpo = ".post-content",
  base       = "https://www.biobiochile.cl",
  plantilla_busqueda = "https://www.biobiochile.cl/buscador.shtml?s={q}"
)
