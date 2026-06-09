# CGR PRENSA — PASO 1: cargar, limpiar, deduplicar y FILTRAR por relevancia CGR
# Lee todos los .rds de scraping/datos/{fuente}/, los unifica en una sola base
# (una noticia por fila), interpreta fechas, limpia texto, aplica el filtro de
# relevancia CGR (términos de la Contraloría y temas afines) y guarda el resultado.
#
# input : scraping/datos/{fuente}/*.rds
# output: datos/cgr_datos.parquet  (solo noticias relevantes a la CGR)
#         datos/cgr_metricas.rds

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(tidyr)
  library(arrow)
})
source("funciones.R")
source("datos/cgr_terminos.R")

tictoc <- Sys.time()

# 1. Cargar todos los .rds del scraping -----------------------------------------
archivos <- list.files("scraping/datos", pattern = "\\.rds$",
                       recursive = TRUE, full.names = TRUE)
message(glue::glue("Archivos .rds encontrados: {length(archivos)}"))

if (length(archivos) == 0) {
  stop("No hay datos scrapeados en scraping/datos/. Ejecuta primero scraping/cgr_scraping.R")
}

crudo <- map(archivos, function(a) {
  d <- tryCatch(readRDS(a), error = function(e) NULL)
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(NULL)
  mutate(d, across(everything(), as.character))
}) |>
  list_rbind()

message(glue::glue("Noticias crudas: {nrow(crudo)}"))

# 2. Limpieza y deduplicación ---------------------------------------------------
datos <- crudo |>
  # columnas mínimas
  filter(!is.na(titulo), nchar(titulo) > 15) |>
  distinct(url, .keep_all = TRUE) |>
  mutate(
    titulo = limpiar_texto_poquito(titulo),
    bajada = limpiar_texto_poquito(bajada),
    cuerpo = limpiar_texto_poquito(cuerpo)
  )

# 3. Fechas ---------------------------------------------------------------------
datos <- datos |>
  mutate(
    fecha2 = suppressWarnings(as_date(fecha)),
    fecha2 = if_else(is.na(fecha2), suppressWarnings(ymd(str_extract(url, "\\d{4}.\\d{2}.\\d{2}"))), fecha2),
    fecha2 = if_else(is.na(fecha2), as_date(ymd_hms(fecha_scraping, quiet = TRUE)), fecha2),
    # fechas implausibles -> fecha de scraping
    fecha2 = if_else(fecha2 < as_date("2000-01-01") | fecha2 > today() + 1,
                     as_date(ymd_hms(fecha_scraping, quiet = TRUE)), fecha2)
  ) |>
  mutate(
    fecha = fecha2,
    anio = year(fecha),
    semana = floor_date(fecha, unit = "week", week_start = 1)
  ) |>
  select(-fecha2)

# 4. Texto para relevancia y análisis -------------------------------------------
datos <- datos |>
  mutate(
    bajada = replace_na(bajada, ""),
    cuerpo = replace_na(cuerpo, ""),
    # filtro de relevancia: SOLO título + bajada (el cuerpo a veces trae
    # contaminación del scraping —menús, "noticias relacionadas"— que menciona
    # "Contraloría" y generaba falsos positivos como "robo a adulta mayor").
    texto_filtro = str_squish(paste(titulo, bajada)),
    texto = str_squish(paste(titulo, bajada, cuerpo))
  )

# 5. FILTRO DE RELEVANCIA CGR ---------------------------------------------------
n_antes <- nrow(datos)
datos <- datos |>
  filter(es_relevante_cgr(texto_filtro)) |>
  mutate(categorias = categorias_cgr(texto)) |>
  select(-texto_filtro)

message(glue::glue("Noticias relacionadas con la CGR: {nrow(datos)} de {n_antes}"))

# 5b. CORTE TEMPORAL: solo desde el inicio del monitoreo ------------------------
# El scraping propio comenzó en mayo de 2026; los buscadores de los medios
# devuelven también notas antiguas, que aquí se descartan para que los análisis
# reflejen solo el período monitoreado (configurable con CGR_FECHA_MIN).
fecha_min <- suppressWarnings(as_date(Sys.getenv("CGR_FECHA_MIN", "2026-05-01")))
if (is.na(fecha_min)) fecha_min <- as_date("2026-05-01")
n_pre <- sum(datos$fecha < fecha_min, na.rm = TRUE)
datos <- datos |> filter(!is.na(fecha), fecha >= fecha_min)
message(glue::glue("Corte temporal (>= {fecha_min}): se descartaron {n_pre}; quedan {nrow(datos)}"))

# 6. Limpieza profunda de texto + id --------------------------------------------
datos_prensa <- datos |>
  mutate(
    cuerpo_limpio = limpiar_texto(cuerpo),
    texto_limpio  = limpiar_texto(texto)
  ) |>
  rowwise() |>
  mutate(id = digest::digest(url, algo = "xxhash64")) |>
  ungroup() |>
  recodificar_fuentes() |>
  select(id, fuente, fecha, anio, semana, titulo, bajada, cuerpo,
         cuerpo_limpio, texto_limpio, categorias, url, fecha_scraping) |>
  arrange(desc(fecha))

# 7. Acumular con el corpus previo ----------------------------------------------
# Los runners de GitHub Actions son efímeros y scraping/datos/ no se versiona,
# por lo que el corpus persiste en datos/cgr_datos.parquet (sí versionado).
# Aquí se une lo nuevo con lo ya acumulado, deduplicando por id.
if (file.exists("datos/cgr_datos.parquet")) {
  previo <- tryCatch(arrow::read_parquet("datos/cgr_datos.parquet"), error = function(e) NULL)
  if (!is.null(previo) && nrow(previo) > 0) {
    n_previo <- nrow(previo)
    datos_prensa <- bind_rows(previo, datos_prensa) |>
      distinct(id, .keep_all = TRUE) |>
      arrange(desc(fecha))
    message(glue::glue("Acumulado: {n_previo} previas + nuevas = {nrow(datos_prensa)} noticias"))
  }
}

# 7b. Solo noticias scrapeadas por nosotros -------------------------------------
# Se descarta cualquier noticia importada de muestras externas (url muestra2025://);
# el corpus contiene únicamente lo recolectado por el scraping propio.
n_corpus <- nrow(datos_prensa)
datos_prensa <- datos_prensa |> filter(startsWith(url, "http"))
message(glue::glue("Solo scrapeadas (sin muestra externa): {nrow(datos_prensa)} de {n_corpus}"))

# 7c. Corte temporal sobre el corpus ACUMULADO ----------------------------------
# El corpus previo puede traer noticias anteriores al inicio del monitoreo
# (acumuladas cuando el corte era más laxo); se aplican el mismo límite.
n_corpus <- nrow(datos_prensa)
datos_prensa <- datos_prensa |> filter(!is.na(fecha), fecha >= fecha_min)
if (n_corpus > nrow(datos_prensa)) {
  message(glue::glue("Corte temporal del acumulado: {n_corpus - nrow(datos_prensa)} descartadas"))
}

# 7d. Deduplicar por titular dentro de cada fuente -------------------------------
# La misma nota puede entrar dos veces con URLs distintas (scraping directo y
# Google News RSS). Se conserva la versión con más texto (bajada/cuerpo).
n_corpus <- nrow(datos_prensa)
datos_prensa <- datos_prensa |>
  mutate(.titulo_norm = stringi::stri_trans_general(tolower(titulo), "Latin-ASCII")) |>
  arrange(desc(nchar(paste(replace_na(bajada, ""), replace_na(cuerpo, ""))))) |>
  distinct(fuente, .titulo_norm, .keep_all = TRUE) |>
  select(-.titulo_norm) |>
  arrange(desc(fecha))
if (n_corpus > nrow(datos_prensa)) {
  message(glue::glue("Dedup por titular: {n_corpus - nrow(datos_prensa)} duplicadas descartadas"))
}

# 8. Guardar --------------------------------------------------------------------
if (!dir.exists("datos")) dir.create("datos")
arrow::write_parquet(datos_prensa, "datos/cgr_datos.parquet")

# Métricas para la app
metricas <- list(
  total          = nrow(datos_prensa),
  semana_actual  = sum(datos_prensa$semana == floor_date(today(), "week", week_start = 1), na.rm = TRUE),
  fuentes_activas = n_distinct(datos_prensa$fuente),
  ultima_fecha   = max(datos_prensa$fecha, na.rm = TRUE),
  actualizado    = now()
)
saveRDS(metricas, "datos/cgr_metricas.rds")

message("\n-- Noticias CGR por fuente --")
print(datos_prensa |> count(fuente, sort = TRUE) |> as.data.frame(), row.names = FALSE)
message(glue::glue("\nP1 listo en {round(difftime(Sys.time(), tictoc, units='secs'),1)} s — datos/cgr_datos.parquet"))
