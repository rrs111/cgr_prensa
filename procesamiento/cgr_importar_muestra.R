# CGR PRENSA — importar una muestra externa de noticias
# Toma un CSV con columnas  titulo;cuerpo;fuente;fecha  (formato de
# prensa_chile, p. ej. la muestra de 10.000 noticias de 2025 de Bastián Olea),
# le aplica el MISMO filtro de relevancia CGR y limpieza que el pipeline, y
# fusiona las noticias relevantes en datos/cgr_datos.parquet (deduplicando).
#
# Uso:
#   Rscript procesamiento/cgr_importar_muestra.R [ruta_csv]
#   (por defecto: datos/prensa_datos_muestra_2025.csv)
#
# Luego ejecuta el pipeline para recalcular tokens/conteos:
#   Rscript cgr_procesar.R

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(purrr)
  library(lubridate); library(readr); library(arrow)
})
source("funciones.R")
source("datos/cgr_terminos.R")

args <- commandArgs(trailingOnly = TRUE)
ruta <- if (length(args) >= 1) args[1] else "datos/prensa_datos_muestra_2025.csv"
stopifnot("No existe el CSV de muestra" = file.exists(ruta))

message(glue::glue("Leyendo muestra: {ruta}"))
muestra <- readr::read_delim(ruta, delim = ";", show_col_types = FALSE,
                             escape_double = TRUE, trim_ws = TRUE)
message(glue::glue("Filas en la muestra: {nrow(muestra)}"))

# columnas mínimas
stopifnot(all(c("titulo", "cuerpo", "fuente", "fecha") %in% names(muestra)))

datos <- muestra |>
  transmute(
    titulo = limpiar_texto_poquito(as.character(titulo)),
    bajada = "",
    cuerpo = limpiar_texto_poquito(as.character(cuerpo)),
    fuente = as.character(fuente),
    fecha  = suppressWarnings(as_date(fecha)),
    fecha_scraping = NA_character_
  ) |>
  filter(!is.na(titulo), nchar(titulo) > 15) |>
  mutate(
    anio = year(fecha),
    semana = floor_date(fecha, unit = "week", week_start = 1),
    texto = str_squish(paste(titulo, bajada, cuerpo))
  )

# FILTRO DE RELEVANCIA CGR
n_antes <- nrow(datos)
datos <- datos |>
  filter(es_relevante_cgr(texto)) |>
  mutate(categorias = categorias_cgr(texto))
message(glue::glue("Noticias CGR en la muestra: {nrow(datos)} de {n_antes}"))

# limpieza profunda + id (mismo esquema que p1)
nuevas <- datos |>
  mutate(
    cuerpo_limpio = limpiar_texto(cuerpo),
    texto_limpio  = limpiar_texto(texto),
    url = paste0("muestra2025://", str_sub(map_chr(paste(titulo, fecha), ~digest::digest(.x, algo = "xxhash64")), 1, 12))
  ) |>
  rowwise() |>
  mutate(id = digest::digest(paste(titulo, fecha, fuente), algo = "xxhash64")) |>
  ungroup() |>
  recodificar_fuentes() |>
  select(id, fuente, fecha, anio, semana, titulo, bajada, cuerpo,
         cuerpo_limpio, texto_limpio, categorias, url, fecha_scraping)

# fusionar con el corpus existente
if (file.exists("datos/cgr_datos.parquet")) {
  previo <- arrow::read_parquet("datos/cgr_datos.parquet")
  n_previo <- nrow(previo)
  combinado <- bind_rows(previo, nuevas) |>
    distinct(id, .keep_all = TRUE) |>
    arrange(desc(fecha))
  message(glue::glue("Corpus: {n_previo} previas + {nrow(nuevas)} de la muestra = {nrow(combinado)} (deduplicado)"))
} else {
  combinado <- nuevas |> arrange(desc(fecha))
}

arrow::write_parquet(combinado, "datos/cgr_datos.parquet")
message(glue::glue("Listo. datos/cgr_datos.parquet ahora tiene {nrow(combinado)} noticias CGR."))
message("Ejecuta `Rscript cgr_procesar.R` para recalcular tokens/conteos para la app.")
