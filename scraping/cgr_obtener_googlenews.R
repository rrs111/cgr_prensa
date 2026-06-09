# CGR Prensa — Google News RSS para medios con paywall o Cloudflare
# Cubre los medios que no se pueden scrapear directo:
#   - La Segunda        (paywall: el sitio solo entrega titular+bajada)
#   - Diario Financiero (paywall + anti-bot)
#   - The Clinic        (Cloudflare, ni Chrome headless pasa)
#   - Ex-Ante           (Cloudflare, ni Chrome headless pasa)
# Google News RSS entrega titular + fecha + enlace (redirect de Google que
# lleva a la nota original). Sin bajada ni cuerpo, pero el titular basta para
# el filtro de relevancia CGR y el monitoreo de cobertura/titulares.
#
# La Segunda se consulta sin keyword (site: completo, ~60-70 ítems en 90 días)
# porque Google indexa poco de ese dominio; el filtro CGR de P1 descarta el
# resto. Los demás se consultan ya acotados a términos CGR.
source("funciones.R")
source("scraping/cgr_scraping_funciones.R")

TERMINOS_GNEWS <- "(contraloria OR contralor OR contralora OR \"dorothy perez\")"

consultas <- list(
  lasegunda = "site:lasegunda.com when:90d",
  df        = paste(TERMINOS_GNEWS, "site:df.cl when:90d"),
  theclinic = paste(TERMINOS_GNEWS, "site:theclinic.cl when:90d"),
  exante    = paste(TERMINOS_GNEWS, "site:ex-ante.cl when:90d")
)

for (f in names(consultas)) {
  buscar_google_news(fuente = f, consulta = consultas[[f]])
  pausar(1, 2)
}
