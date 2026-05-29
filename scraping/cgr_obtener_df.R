# CGR Prensa — scraping de Diario Financiero (df.cl)
# Verificado en vivo 2026-05-28. DF tiene paywall y protección anti-bot:
# el cuerpo suele no estar disponible; se capturan título + bajada + fecha.
# Las URLs de noticia tienen forma /noticias/site/artic/YYYYMMDD/pags/NNN.html
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

ejecutar_modulo(
  fuente = "df",
  secciones_reciente = c(
    "https://www.df.cl/",
    "https://www.df.cl/mercados",
    "https://www.df.cl/empresas"
  ),
  patron     = "/noticias/site/artic/\\d{8}/pags/\\d+\\.html$",
  sel_cuerpo = "article",   # paywall: la señal va en título/bajada
  base       = "https://www.df.cl"
)
