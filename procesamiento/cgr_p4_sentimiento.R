# CGR PRENSA — PASO 4: tono / sentimiento de la cobertura
# Calcula el TONO de cada noticia con un léxico de polaridad en español
# (datos/cgr_lexico_sentimiento.csv, derivado de NRC-es + términos del dominio).
# Mide qué tan cargado/negativo es el lenguaje de la cobertura sobre la CGR
# (no el sentimiento *hacia* la CGR: una nota donde la CGR destapa corrupción
#  tendrá tono negativo por el contexto del hecho).
#
# input : datos/cgr_palabras.parquet (paso 2), datos/cgr_datos.parquet (paso 1)
# output: datos/cgr_tono_articulo.parquet   (id, score, clase)
#         datos/cgr_tono_semana.parquet      (tono + índice de presión mediática)
#         datos/cgr_tono_fuente.parquet      (tono por medio)

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(arrow); library(lubridate)
})
source("funciones.R")

tini <- Sys.time()

if (!exists("palabras"))     palabras     <- arrow::read_parquet("datos/cgr_palabras.parquet")
if (!exists("datos_prensa")) datos_prensa <- arrow::read_parquet("datos/cgr_datos.parquet")

lexico <- readr::read_csv("datos/cgr_lexico_sentimiento.csv", show_col_types = FALSE)

# 1. Tono por noticia ----------------------------------------------------------
# umbrales: una noticia es negativa/positiva si el balance supera ±0.2,
# siempre que tenga al menos 3 palabras con polaridad (si no, neutro).
umbral_score <- 0.20
min_lex      <- 3L

puntajes <- palabras |>
  inner_join(lexico, by = "palabra") |>
  group_by(id) |>
  summarise(
    n_pos = sum(polaridad > 0),
    n_neg = sum(polaridad < 0),
    .groups = "drop"
  ) |>
  mutate(
    n_lex = n_pos + n_neg,
    score = ifelse(n_lex > 0, (n_pos - n_neg) / n_lex, 0)
  )

tono_articulo <- datos_prensa |>
  select(id, fuente, fecha, semana) |>
  left_join(puntajes, by = "id") |>
  mutate(
    across(c(n_pos, n_neg, n_lex), ~tidyr::replace_na(.x, 0L)),
    score = tidyr::replace_na(score, 0),
    clase = case_when(
      n_lex < min_lex          ~ "neutro",
      score <= -umbral_score   ~ "negativo",
      score >=  umbral_score   ~ "positivo",
      TRUE                     ~ "neutro"
    )
  )

arrow::write_parquet(tono_articulo |> select(id, score, n_lex, clase),
                     "datos/cgr_tono_articulo.parquet")

# 2. Tono por semana + índice de presión mediática -----------------------------
# Índice de presión = nº de noticias de tono negativo en la semana
# (volumen de cobertura negativa: combina cuánto se habla y qué tan crítico es).
tono_semana <- tono_articulo |>
  group_by(semana) |>
  summarise(
    n_noticias = n(),
    score_prom = round(mean(score), 3),
    pct_neg = round(mean(clase == "negativo"), 3),
    pct_neu = round(mean(clase == "neutro"), 3),
    pct_pos = round(mean(clase == "positivo"), 3),
    .groups = "drop"
  ) |>
  mutate(indice_presion = round(n_noticias * pct_neg)) |>
  arrange(semana)

arrow::write_parquet(tono_semana, "datos/cgr_tono_semana.parquet")

# 3. Tono por fuente -----------------------------------------------------------
tono_fuente <- tono_articulo |>
  group_by(fuente) |>
  summarise(
    n = n(),
    score_prom = round(mean(score), 3),
    pct_neg = round(mean(clase == "negativo"), 3),
    pct_pos = round(mean(clase == "positivo"), 3),
    .groups = "drop"
  ) |>
  filter(n >= 3) |>
  arrange(score_prom)

arrow::write_parquet(tono_fuente, "datos/cgr_tono_fuente.parquet")

message(glue::glue(
  "P4 (tono) listo en {round(difftime(Sys.time(), tini, units='secs'),1)} s\n",
  "  noticias con tono: neg {sum(tono_articulo$clase=='negativo')}, ",
  "neu {sum(tono_articulo$clase=='neutro')}, pos {sum(tono_articulo$clase=='positivo')}\n",
  "  score promedio global: {round(mean(tono_articulo$score),3)}"
))
