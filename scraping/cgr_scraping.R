# CGR Prensa — ORQUESTADOR DE SCRAPING
# Ejecuta secuencialmente todos los módulos cgr_obtener_*.R.
# Compatible con RStudio y con `Rscript scraping/cgr_scraping.R` en terminal
# (NO usa callr ni rstudioapi).
#
# Modo de uso (variable CGR_MODO):
#   completo (def.) = busca términos CGR en el buscador de cada medio + secciones
#   buscar          = solo búsqueda de términos CGR (máxima cobertura temática)
#   reciente        = solo secciones recientes (nacional/política/economía)
#   hist            = secciones con paginación hacia atrás
#
#   Rscript scraping/cgr_scraping.R                       # completo, todas las fuentes
#   CGR_MODO=buscar Rscript scraping/cgr_scraping.R       # solo búsquedas CGR
#   CGR_MAX=20 Rscript scraping/cgr_scraping.R            # tope de noticias por fuente
#   CGR_FUENTES="emol,biobio" Rscript ...                # solo algunas fuentes
#
# El scraping por búsqueda surte candidatos; el filtro fino de relevancia CGR
# se aplica en el procesamiento (procesamiento/cgr_p1_cargar_datos.R).

suppressPackageStartupMessages({
  library(glue)
  library(purrr)
  library(lubridate)
})
source("funciones.R")

# Orden de ejecución -----------------------------------------------------------
# Primero las fuentes que funcionan con rvest (rápidas y confiables);
# luego paywall (titular+bajada); al final las que requieren chromote/Chrome.
modulos_rvest <- c("emol", "biobio", "cooperativa", "cnnchile", "elmostrador",
                   "eldinamo", "ciper", "pauta", "t13", "interferencia",
                   "ellibero", "eldesconcierto")
modulos_paywall <- c("lasegunda")
modulos_js <- c("latercera", "theclinic")

# Fuentes DESHABILITADAS por dar 0 noticias de forma garantizada:
#   - df     : paywall de Diario Financiero (no se obtiene cuerpo ni titular útil)
#   - exante : bloqueado por Cloudflare incluso con Chrome headless
# Sus módulos siguen existiendo (cgr_obtener_df.R / _exante.R) y pueden correrse
# a mano con CGR_FUENTES="df,exante" si algún día cambian de plataforma.
modulos_deshabilitados <- c("df", "exante")

modulos <- c(modulos_rvest, modulos_paywall, modulos_js)

# Permitir limitar a algunas fuentes vía variable de entorno.
# Si se piden explícitamente, se permite incluso reactivar las deshabilitadas
# (ej. CGR_FUENTES="df,exante" para una prueba puntual).
sel <- Sys.getenv("CGR_FUENTES", "")
if (nzchar(sel)) {
  pedidas <- trimws(strsplit(sel, ",")[[1]])
  modulos <- intersect(pedidas, c(modulos, modulos_deshabilitados))
}

# Ejecución --------------------------------------------------------------------
notificacion("CGR Prensa", glue("Iniciando scraping ({Sys.getenv('CGR_MODO','reciente')})…"))
inicio <- now()

correr <- function(fuente) {
  archivo <- glue("scraping/cgr_obtener_{fuente}.R")
  if (!file.exists(archivo)) { message("(no existe ", archivo, ")"); return(invisible()) }
  tryCatch(
    source(archivo, local = new.env()),
    error = function(e) message("ERROR en módulo ", fuente, ": ", conditionMessage(e))
  )
}

walk(modulos, correr)

# Resumen ----------------------------------------------------------------------
message("\n========== RESUMEN SCRAPING ==========")
if (dir.exists("scraping/datos")) {
  carpetas <- list.dirs("scraping/datos", recursive = FALSE)
  resumen <- map(carpetas, function(carp) {
    archivos_hoy <- list.files(carp, pattern = glue("{today()}.*\\.rds$"), full.names = TRUE)
    n <- if (length(archivos_hoy)) sum(map_int(archivos_hoy, ~tryCatch(nrow(readRDS(.x)), error = function(e) 0L))) else 0L
    data.frame(fuente = basename(carp), archivos_hoy = length(archivos_hoy), noticias_hoy = n)
  })
  if (length(resumen)) print(do.call(rbind, resumen), row.names = FALSE)
}

tiempo <- round(difftime(now(), inicio, units = "mins"), 1)
notificacion("CGR Prensa", glue("Scraping finalizado en {tiempo} min."))
