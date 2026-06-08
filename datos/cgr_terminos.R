# Términos de relevancia CGR y stopwords del dominio.
# Se usa en el procesamiento (NO en el scraping): el scraping baja todo y aquí
# se filtran las noticias relacionadas con la Contraloría y temas afines.

suppressPackageStartupMessages({
  library(stringr)
  library(stringi)
  library(purrr)
})

# —--------------------------------------------------------------------------
# Términos de relevancia, agrupados por categoría (para reportes/etiquetado)
# —--------------------------------------------------------------------------

terminos_cgr <- list(
  institucional = c(
    "contraloria", "contralor", "contralora", "cgr", "dorothy perez",
    "contraloria general de la republica", "ente contralor", "organo contralor"
  ),
  funciones = c(
    "toma de razon", "dictamen", "dictamenes", "fiscalizacion", "fiscalizar",
    "auditoria publica", "auditoria", "sumario administrativo", "sumario",
    "responsabilidad administrativa", "juicio de cuentas", "rendicion de cuentas",
    "representacion", "reparo"
  ),
  tematica = c(
    "probidad", "transparencia publica", "gasto publico", "fondos publicos",
    "integridad publica", "corrupcion", "anticorrupcion", "malversacion",
    "peculado", "cohecho", "soborno", "fraude al fisco", "irregularidad",
    "irregularidades", "lobby", "declaracion de intereses",
    "declaracion de patrimonio", "ley de transparencia",
    "consejo para la transparencia", "contraloria regional",
    "funcionarios publicos", "glosa presupuestaria", "presupuesto publico",
    "uso de recursos publicos", "enriquecimiento ilicito", "conflicto de interes"
  ),
  regional = c(
    "gobierno regional", "gobiernos regionales", "gobernador regional",
    "gobernadora regional", "consejo regional", "core regional",
    "subdere", "municipalidad", "rendicion de cuentas municipal"
  )
)

# —--------------------------------------------------------------------------
# Términos para el SCRAPING POR BÚSQUEDA (se consultan en el buscador de cada
# medio). Son un subconjunto de alto rendimiento; el filtro de relevancia
# posterior (es_relevante_cgr) descarta los falsos positivos.
# —--------------------------------------------------------------------------
terminos_busqueda <- c(
  "contraloria", "contralor", "dorothy perez", "toma de razon",
  "probidad", "fiscalizacion", "gobierno regional", "corrupcion",
  "auditoria", "dictamen"
)

# Vector plano de todos los términos (sin tildes, en minúscula)
terminos_cgr_todos <- unique(unlist(terminos_cgr, use.names = FALSE))

# —--------------------------------------------------------------------------
# Normalización y regex de relevancia
# —--------------------------------------------------------------------------

# Normaliza texto: minúsculas + sin tildes/diacríticos, para comparar de forma robusta
normalizar_texto <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower()
}

# —--------------------------------------------------------------------------
# NÚCLEO INSTITUCIONAL: una noticia es relevante SOLO si menciona explícitamente
# a la Contraloría o a una de sus funciones inequívocas. Los términos temáticos
# amplios (corrupción, probidad, transparencia, etc.) NO bastan por sí solos
# para incluir una noticia — solo aportan contexto/etiqueta una vez que ya
# entró por el núcleo. Esto evita falsos positivos (columnas de opinión, notas
# de "salud pública", etc. que no hablan de la CGR).
#
# Se usan prefijos (sin \\b final) para cubrir todas las flexiones:
#   "contralor"  -> contraloría, contralor, contralora, contralores, contralorías
#
# IMPORTANTE: solo se incluyen términos INEQUÍVOCOS de la CGR. Se quitaron a
# propósito términos que otros órganos también usan y producían falsos
# positivos:
#   - "dictamen": lo emiten tribunales (incl. deportivos), cortes, fiscalía...
#     (ej. "Cobreloa vs Colo Colo", "ANFP castigó a jugador").
#   - "sumario administrativo" / "responsabilidad administrativa": los instruye
#     cualquier servicio público y la Corte (ej. "Suprema confirma suspensión").
# Las noticias de dictámenes/sumarios DE la CGR igual entran, porque mencionan
# "Contraloría" (capturado por "contralor").
# —--------------------------------------------------------------------------
nucleo_cgr <- c(
  "contralor",                       # contraloría/contralor/contralora/regional/general
  "dorothy perez",
  "toma de razon",                   # trámite exclusivo de la CGR
  "juicio de cuentas"                # procedimiento exclusivo de la CGR
)

# regex del núcleo: "cgr" como palabra completa; prefijos sin límite final;
# multipalabra con espacios flexibles.
.nucleo_sin_cgr <- nucleo_cgr
nucleo_cgr_regex <- paste0(
  "\\bcgr\\b|",
  paste0("\\b", str_replace_all(.nucleo_sin_cgr, " ", "\\\\s+"), collapse = "|")
)

# (se conserva el regex amplio por compatibilidad / usos futuros)
.terminos_sin_cgr <- setdiff(terminos_cgr_todos, "cgr")
terminos_cgr_regex <- paste0(
  "\\bcgr\\b|",
  paste0("\\b", str_replace_all(.terminos_sin_cgr, " ", "\\\\s+"), "\\b", collapse = "|")
)

# Devuelve TRUE si el texto menciona explícitamente a la CGR (núcleo institucional).
es_relevante_cgr <- function(texto) {
  texto_norm <- normalizar_texto(texto)
  str_detect(texto_norm, nucleo_cgr_regex)
}

# Devuelve, para cada texto, qué categorías CGR menciona (string separado por ;)
categorias_cgr <- function(texto) {
  texto_norm <- normalizar_texto(texto)
  map_chr(texto_norm, function(t) {
    cats <- names(terminos_cgr)[map_lgl(terminos_cgr, function(grupo) {
      patron <- paste0("\\b", str_replace_all(grupo, " ", "\\\\s+"), "\\b", collapse = "|")
      str_detect(t, patron)
    })]
    if (length(cats) == 0) NA_character_ else paste(cats, collapse = ";")
  })
}

# —--------------------------------------------------------------------------
# Stopwords propias del dominio (se suman a las stopwords del español)
# Palabras muy frecuentes que no aportan al análisis temático CGR.
# —--------------------------------------------------------------------------

stopwords_propias <- c(
  "chile", "chilena", "chileno", "pais", "nacional", "region", "regional",
  "ano", "anos", "dia", "dias", "hoy", "ayer", "manana", "semana", "mes",
  "senalo", "senala", "indico", "indica", "agrego", "sostuvo", "afirmo",
  "explico", "aseguro", "manifesto", "expreso", "comento", "detallo",
  "segun", "ademas", "asi", "tras", "luego", "ahora", "tambien", "cabe",
  "millones", "mil", "pesos", "ciento", "porciento",
  "informacion", "comunicado", "declaracion", "publicacion", "articulo",
  "noticia", "noticias", "medio", "medios", "prensa", "reportaje",
  "foto", "fotografia", "imagen", "video", "lectura", "leer",
  "twitter", "facebook", "instagram", "whatsapp", "telegram",
  "señor", "señora", "persona", "personas", "gente"
)
