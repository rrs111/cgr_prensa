# CGR PRENSA — PASO 7: red de co-ocurrencia de términos
# Identifica qué palabras aparecen juntas dentro de una ventana de contexto.
# A diferencia de la correlación a nivel de noticia, captura asociaciones
# locales ("toma + razón", "licencia + médica", "fondos + públicos").
#
# Estrategia:
#   - Tokenizar titulo + bajada + cuerpo (normalizado) con {quanteda}.
#   - Quitar stopwords ES + propias + dominio (las mismas que en p5).
#   - Construir FCM (feature co-occurrence matrix) con ventana = 5 tokens.
#   - Quedarse con los pares más fuertes y precomputar el layout de la red
#     (Fruchterman-Reingold con {igraph}) para los top N nodos, así la app
#     solo dibuja.
#
# input : datos/cgr_datos.parquet
# output: datos/cgr_coocurrencia.parquet (palabra_a, palabra_b, peso)
#         datos/cgr_red_nodos.parquet    (palabra, x, y, peso)
#         datos/cgr_red_aristas.parquet  (palabra_a, palabra_b, peso, x0, y0, x1, y1)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(arrow); library(lubridate)
  library(quanteda); library(igraph)
})
source("funciones.R")
source("datos/cgr_terminos.R")

tini <- Sys.time()

# 1. Texto -----------------------------------------------------------------------
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/cgr_datos.parquet")

corpus_df <- datos_prensa |>
  mutate(
    cuerpo_limpio = tidyr::replace_na(cuerpo_limpio, ""),
    bajada = tidyr::replace_na(bajada, ""),
    texto  = normalizar_texto(str_squish(paste(titulo, bajada, cuerpo_limpio)))
  ) |>
  filter(nchar(texto) >= 60)

# 2. Stopwords (ES + propias + dominio CGR) -------------------------------------
sw_es <- normalizar_texto(tm::stopwords("spanish"))
sw_dom <- normalizar_texto(c(
  "contraloria", "contralor", "contralora", "cgr", "general", "republica",
  "indico", "indica", "senalo", "senala", "afirmo", "afirma", "sostuvo",
  "explico", "agrego", "aseguro", "manifesto", "expreso", "comento",
  "detallo", "dijo", "dice", "advirtio", "tras", "ademas",
  "ano", "anos", "dia", "dias", "hoy", "ayer", "mientras", "luego",
  "pais", "chile", "chilena", "nacional", "millones", "mil",
  "solo", "asi", "asimismo", "tambien", "tal", "mismo", "misma",
  "uno", "dos", "tres", "primer", "segunda", "tercera", "ultimo",
  "ultima", "hizo", "hace", "haciendo", "sera", "ser"
))
custom_sw <- unique(c(sw_es, sw_dom, normalizar_texto(stopwords_propias)))

# 3. Tokenizar + FCM ------------------------------------------------------------
toks <- tokens(corpus_df$texto, remove_punct = TRUE, remove_numbers = TRUE,
               remove_symbols = TRUE)
toks <- tokens_remove(toks, custom_sw, padding = FALSE)
toks <- tokens_select(toks, min_nchar = 3, max_nchar = 22)

fcm_mat <- fcm(toks, context = "window", window = 5L, count = "weighted",
               weights = 1 / (1:5))  # más cerca pesa más

# 4. Top features y reducción para el grafo -------------------------------------
dfm_aux <- dfm(toks)
top_feats <- topfeatures(dfm_aux, n = 200)
nodos_top <- names(top_feats)
fcm_sub <- fcm_select(fcm_mat, pattern = nodos_top)

# matriz triangular → tibble largo
m <- as.matrix(fcm_sub)
m[lower.tri(m, diag = TRUE)] <- 0
idx <- which(m > 0, arr.ind = TRUE)
coocurrencia <- tibble(
    palabra_a = rownames(m)[idx[, 1]],
    palabra_b = colnames(m)[idx[, 2]],
    peso      = m[idx]
  ) |>
  arrange(desc(peso)) |>
  head(1500)

arrow::write_parquet(coocurrencia, "datos/cgr_coocurrencia.parquet")

# 5. Subgrafo para la red visual (top 50 nodos por fuerza) ----------------------
fuerza <- coocurrencia |>
  pivot_longer(c(palabra_a, palabra_b), values_to = "palabra") |>
  group_by(palabra) |>
  summarise(peso = sum(peso), .groups = "drop") |>
  arrange(desc(peso)) |>
  head(50)

nodos_top50 <- fuerza$palabra

aristas <- coocurrencia |>
  filter(palabra_a %in% nodos_top50, palabra_b %in% nodos_top50) |>
  arrange(desc(peso)) |>
  head(250)

# 6. Layout (Fruchterman-Reingold) ----------------------------------------------
g <- graph_from_data_frame(
  aristas |> transmute(from = palabra_a, to = palabra_b, weight = peso),
  vertices = fuerza |> filter(palabra %in% c(aristas$palabra_a, aristas$palabra_b)),
  directed = FALSE
)
set.seed(2026)
coords <- layout_with_fr(g, weights = E(g)$weight)
nodos <- tibble(
  palabra = V(g)$name,
  x = coords[, 1],
  y = coords[, 2],
  peso = V(g)$peso
)
aristas_xy <- aristas |>
  left_join(nodos |> select(palabra, x0 = x, y0 = y), by = c("palabra_a" = "palabra")) |>
  left_join(nodos |> select(palabra, x1 = x, y1 = y), by = c("palabra_b" = "palabra"))

arrow::write_parquet(nodos, "datos/cgr_red_nodos.parquet")
arrow::write_parquet(aristas_xy, "datos/cgr_red_aristas.parquet")

message(glue::glue(
  "P7 (red) listo en {round(difftime(Sys.time(), tini, units='secs'),1)} s\n",
  "  coocurrencias: {nrow(coocurrencia)} | nodos red: {nrow(nodos)} | aristas red: {nrow(aristas_xy)}"
))
print(head(coocurrencia, 12))
