# CGR PRENSA — PASO 6: actores / entidades nombradas (NER con {udpipe})
# Extrae personas, instituciones, lugares y cargos mencionados en las noticias.
# Para un órgano fiscalizador es la pieza más valiosa: qué municipios,
# ministerios, fiscalías, alcaldes o autoridades concentran la cobertura.
#
# Estrategia (sin LLM / sin API):
#   - Anotar título + bajada con udpipe (POS tagging) usando el modelo spanish-gsd.
#     Se evita el cuerpo para mantener tiempos razonables; los titulares y leads
#     capturan a los actores principales de cada artículo.
#   - Agrupar tokens PROPN consecutivos como entidades.
#   - Clasificar por reglas (organización si empieza con marcador institucional
#     o es un acrónimo en mayúsculas; lugar si coincide con región/comuna chilena
#     conocida; persona en otro caso).
#   - Extraer cargos como sustantivos comunes (NOUN) de una lista controlada.
#   - Excluir la propia CGR del listado (es el sujeto, no aporta).
#
# input : datos/cgr_datos.parquet  +  datos/modelos/spanish-gsd-*.udpipe
# output: datos/cgr_entidades.parquet           (id, entidad, tipo)
#         datos/cgr_entidades_resumen.parquet   (entidad, tipo, n_menciones, n_fuentes)
#         datos/cgr_entidades_semana.parquet    (semana, entidad, tipo, n)

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(purrr); library(tidyr)
  library(arrow); library(lubridate)
  library(udpipe)
})
source("funciones.R")
source("datos/cgr_terminos.R")

tini <- Sys.time()

# 1. Modelo udpipe ---------------------------------------------------------------
dir.create("datos/modelos", recursive = TRUE, showWarnings = FALSE)
ruta_mod <- list.files("datos/modelos", pattern = "spanish.*\\.udpipe$", full.names = TRUE)
if (length(ruta_mod) == 0) {
  message("Descargando modelo udpipe español (una vez)…")
  m <- udpipe::udpipe_download_model(language = "spanish-gsd", model_dir = "datos/modelos")
  ruta_mod <- m$file_model
} else {
  ruta_mod <- ruta_mod[1]
}
modelo <- udpipe::udpipe_load_model(ruta_mod)

# 2. Documentos: titulo + bajada -----------------------------------------------
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/cgr_datos.parquet")

docs <- datos_prensa |>
  mutate(
    bajada = tidyr::replace_na(bajada, ""),
    texto  = str_squish(paste(titulo, bajada))
  ) |>
  filter(nchar(texto) >= 20) |>
  select(id, fuente, semana, fecha, texto)

message(glue::glue("Documentos a anotar: {nrow(docs)}"))

# 3. Anotación (usa varios hilos si está disponible) ----------------------------
anot <- udpipe::udpipe_annotate(
  modelo, x = docs$texto, doc_id = docs$id,
  parser = "none",                              # no necesitamos dependencias
  parallel.cores = max(1L, parallel::detectCores() - 1L)
) |> as.data.frame()

message(glue::glue("Tokens anotados: {nrow(anot)}"))

# 4. Listas de control ----------------------------------------------------------
markers_org <- c(
  "Ministerio", "Servicio", "Corte", "Tribunal", "Comisión", "Consejo",
  "Cámara", "Senado", "Banco", "Universidad", "Carabineros", "Fuerza",
  "Fuerzas", "Ejército", "Armada", "Fundación", "Asociación", "Federación",
  "Sindicato", "Partido", "Fiscalía", "Defensoría", "Policía",
  "Superintendencia", "Subsecretaría", "Dirección", "Departamento",
  "Empresa", "Sociedad", "Grupo", "Constructora", "Frente", "Confederación",
  "Colegio", "Instituto", "Centro", "Movimiento", "Coordinadora"
)

# Acronimos all-caps 2-6 (CGR ya excluido)
acronimo_re <- "^[A-ZÑ]{2,6}$"

# Lugares: regiones + algunas comunas grandes (catálogo controlado)
lugares_chile <- c(
  "Arica", "Iquique", "Antofagasta", "Calama", "Copiapó", "La Serena",
  "Coquimbo", "Valparaíso", "Viña", "San Antonio", "Rancagua", "Talca",
  "Chillán", "Concepción", "Talcahuano", "Temuco", "Valdivia", "Osorno",
  "Puerto", "Coyhaique", "Punta", "Santiago", "Maipú", "Puente Alto",
  "Las Condes", "Providencia", "Ñuñoa", "La Florida", "La Reina",
  "Lo Barnechea", "Vitacura", "Macul", "Peñalolén", "Recoleta",
  "Independencia", "Quilicura", "Renca", "Cerro Navia", "Pudahuel",
  "Estación Central", "San Bernardo", "La Pintana", "Quinta Normal",
  "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
  "Metropolitana", "O'Higgins", "Maule", "Ñuble", "Biobío", "Araucanía",
  "Aysén", "Magallanes", "Los Lagos", "Los Ríos",
  "Chile", "Argentina", "Perú", "Bolivia", "Venezuela", "Colombia",
  "Brasil", "Ecuador", "Uruguay", "Paraguay", "España", "Estados Unidos",
  "EE.UU.", "China", "Rusia", "Europa", "Latinoamérica"
)
lugares_norm <- normalizar_texto(lugares_chile)

# Excluir la CGR como entidad (es el sujeto), nombres de los medios (venue,
# no actor) y palabras espurias.
excluir <- normalizar_texto(c(
  "Contraloría", "Contralor", "Contralora", "CGR", "República",
  "Chile", "Nación", "Estado", "Gobierno",
  "Sr", "Sra", "Don", "Doña",
  # nombres de los medios fuentes -- no son actores de la noticia
  "Emol", "La Tercera", "La Segunda", "Radio BíoBío", "Ciper", "CIPER Chile",
  "El Mostrador", "Cooperativa", "D. Financiero", "Diario Financiero",
  "Ex-Ante", "CNN Chile", "T13", "Pauta", "El Dínamo", "El Desconcierto",
  "Interferencia", "El Líbero", "The Clinic", "BioBioChile", "Meganoticias",
  "La Nación", "Agricultura", "Publimetro", "Radio U. de Ch.",
  "El Ciudadano", "24 Horas", "ADN Radio", "CHV Noticias", "La Hora",
  "La Cuarta", "La Izquierda Diario", "El Siglo"
))

# Cargos: sustantivos comunes de interés (no salen como PROPN)
cargos <- c(
  "contralor", "contralora", "ministro", "ministra", "subsecretario",
  "subsecretaria", "alcalde", "alcaldesa", "gobernador", "gobernadora",
  "presidente", "presidenta", "fiscal", "senador", "senadora",
  "diputado", "diputada", "intendente", "delegado", "delegada",
  "concejal", "concejala", "seremi", "director", "directora",
  "superintendente", "defensor", "defensora", "juez", "jueza",
  "ministro", "ministra"
)

# 5. Entidades PROPN: agrupar tokens consecutivos --------------------------------
propn <- anot |>
  filter(upos == "PROPN") |>
  mutate(token_id = as.integer(token_id)) |>
  arrange(doc_id, sentence_id, token_id) |>
  group_by(doc_id, sentence_id) |>
  mutate(grupo = cumsum(c(1L, diff(token_id) != 1L))) |>
  ungroup()

entidades_propn <- propn |>
  group_by(doc_id, sentence_id, grupo) |>
  summarise(
    entidad = paste(token, collapse = " "),
    n_tokens = n(),
    primer = first(token),
    .groups = "drop"
  ) |>
  filter(nchar(entidad) >= 3) |>
  mutate(
    entidad = str_squish(str_remove_all(entidad, "[\"'`«»“”]")),
    entidad_norm = normalizar_texto(entidad)
  ) |>
  filter(!entidad_norm %in% excluir)

# 6. Clasificar tipo (organización / lugar / persona) ----------------------------
clasif <- entidades_propn |>
  mutate(
    es_acronimo_full  = str_detect(entidad, acronimo_re) & n_tokens == 1,
    es_acronimo_inicio = str_detect(primer, acronimo_re),   # ej. "CIPER Chile"
    es_lugar    = entidad_norm %in% lugares_norm,
    es_org      = primer %in% markers_org | es_acronimo_full | es_acronimo_inicio,
    tipo = case_when(
      es_org   ~ "organizacion",
      es_lugar ~ "lugar",
      TRUE     ~ "persona"
    )
  ) |>
  select(doc_id, entidad, entidad_norm, tipo)

# 7. Cargos (NOUN de la lista controlada) ---------------------------------------
cargos_norm <- normalizar_texto(cargos)
entidades_cargo <- anot |>
  filter(upos == "NOUN") |>
  mutate(lema_norm = normalizar_texto(lemma)) |>
  filter(lema_norm %in% cargos_norm) |>
  transmute(doc_id, entidad = str_to_lower(lemma), entidad_norm = lema_norm,
            tipo = "cargo")

# 8. Unir, normalizar formas y elegir display canónico --------------------------
todas <- bind_rows(clasif, entidades_cargo) |>
  rename(id = doc_id) |>
  distinct(id, entidad_norm, tipo, .keep_all = TRUE)

# Display form = la variante más frecuente por entidad_norm+tipo
display <- todas |>
  count(entidad_norm, tipo, entidad, name = "freq") |>
  group_by(entidad_norm, tipo) |>
  slice_max(freq, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(entidad_norm, tipo, entidad_display = entidad)

entidades <- todas |>
  left_join(display, by = c("entidad_norm", "tipo")) |>
  transmute(id, entidad = entidad_display, tipo)

arrow::write_parquet(entidades, "datos/cgr_entidades.parquet")

# 9. Resumen global por entidad -------------------------------------------------
meta_doc <- datos_prensa |> select(id, fuente, semana, fecha)
ent_meta <- entidades |> left_join(meta_doc, by = "id")

resumen <- ent_meta |>
  group_by(entidad, tipo) |>
  summarise(
    n_menciones = n_distinct(id),
    n_fuentes   = n_distinct(fuente),
    .groups = "drop"
  ) |>
  filter(n_menciones >= 2) |>          # ruido: descartar menciones únicas
  arrange(desc(n_menciones))

arrow::write_parquet(resumen, "datos/cgr_entidades_resumen.parquet")

# 10. Series semanales (top 50 entidades por menciones) ------------------------
top_ent <- resumen |> slice_max(n_menciones, n = 50) |> pull(entidad)
semana_tab <- ent_meta |>
  filter(entidad %in% top_ent) |>
  group_by(semana, entidad, tipo) |>
  summarise(n = n_distinct(id), .groups = "drop") |>
  arrange(semana, desc(n))

arrow::write_parquet(semana_tab, "datos/cgr_entidades_semana.parquet")

# Mensaje final
top10 <- resumen |> slice_max(n_menciones, n = 10)
message(glue::glue(
  "P6 (actores) listo en {round(difftime(Sys.time(), tini, units='secs'),1)} s\n",
  "  Entidades únicas: {nrow(resumen)} (≥2 menciones) ",
  "| Tipos: {paste(names(table(resumen$tipo)), table(resumen$tipo), sep='=', collapse=', ')}"
))
print(top10 |> as.data.frame())
