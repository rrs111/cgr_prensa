# Despliegue de la app CGR Prensa a shinyapps.io
# Requiere la cuenta de shinyapps.io ya configurada con:
#   rsconnect::setAccountInfo(name=..., token=..., secret=...)
# (se obtiene en https://www.shinyapps.io/admin/#/tokens)
#
# La app desplegada NO incluye la carpeta datos/: lee los parquet/rds
# directamente del repo público en GitHub (ver CGR_DATA_URL en app/app.R),
# así se actualiza sola cada vez que el bot diario pushea datos nuevos.
#
# Uso:  Rscript deploy_app.R

library(rsconnect)

deployApp(
  appDir      = "app",                 # solo se sube app.R + www/
  appName     = "cgr_prensa",
  appTitle    = "CGR Prensa — Monitor de Medios",
  account     = "renators",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
