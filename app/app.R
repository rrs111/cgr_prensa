# ============================================================================
# CGR Prensa — App Shiny (Monitor de Medios sobre la Contraloría)
# Estética corporativa CGR: Navy / Teal / Rosa / Crema · DM Sans + DM Serif
# Gráficos interactivos con {plotly}. Funciona con datos reales del pipeline
# o, si no existen, con datos sintéticos de demostración.
# ============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(plotly)
  library(DT)
  library(shinyWidgets)
  library(shinycssloaders)
  library(htmltools)
})

# NOTA: la nube de palabras se dibuja con {plotly}, NO con {wordcloud2}. El
# paquete wordcloud2 inyecta dependencias JS (htmlwidgets/jQuery) antiguas que,
# al convivir con plotly en la misma página, dejan los gráficos plotly en blanco.

# ---------------------------------------------------------------------------
# Paleta y helpers de estilo
# ---------------------------------------------------------------------------
COL_NAVY  <- "#1B1F49"
COL_TEAL  <- "#74CEC4"
COL_ROSA  <- "#F2567A"
COL_CREMA <- "#F4F2E5"
PALETA_CGR <- c(COL_NAVY, COL_TEAL, COL_ROSA, "#3E6E8C", "#C99700", "#7B5EA7")

options(spinner.type = 8, spinner.color = COL_TEAL)

estilo_plotly <- function(p, leyenda = TRUE) {
  p |>
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(family = "DM Sans, sans-serif", color = COL_NAVY, size = 13),
      xaxis = list(gridcolor = "rgba(27,31,73,0.08)", zeroline = FALSE),
      yaxis = list(gridcolor = "rgba(27,31,73,0.08)", zeroline = FALSE),
      showlegend = leyenda,
      legend = list(orientation = "h", y = -0.2),
      margin = list(t = 30, r = 10, b = 40, l = 50),
      hoverlabel = list(font = list(family = "DM Sans, sans-serif"))
    ) |>
    config(displayModeBar = FALSE)
}

# Nube de palabras hecha con plotly (sin dependencia de wordcloud2).
# Distribuye las palabras en una espiral, con tamaño proporcional a la frecuencia.
# Nube de palabras hecha con plotly (sin wordcloud2). Coloca cada palabra en una
# espiral buscando la primera posición SIN colisión con las ya puestas (como las
# nubes de verdad), estimando el "cuadro" de cada palabra por su largo y tamaño.
nube_plotly <- function(d, n_palabras = 45) {
  d <- d |> arrange(desc(freq)) |> head(n_palabras)
  k <- nrow(d)
  if (k == 0) return(plotly_empty())

  fmin <- min(d$freq); fmax <- max(d$freq)
  size <- if (fmax > fmin) {
    12 + 30 * (sqrt(d$freq) - sqrt(fmin)) / (sqrt(fmax) - sqrt(fmin))
  } else rep(20, k)

  # cuadro delimitador aproximado de cada palabra, en "unidades" = px de fuente
  w <- nchar(d$palabra) * size * 0.60   # ancho ≈ nº letras × tamaño
  h <- size * 1.25                       # alto ≈ tamaño (con interlínea)

  px <- numeric(k); py <- numeric(k)
  cx0 <- numeric(0); cy0 <- numeric(0); cw <- numeric(0); ch <- numeric(0)

  for (i in seq_len(k)) {
    t <- 0
    repeat {
      r   <- 0.8 * t                     # radio crece despacio = nube compacta
      ang <- t * 0.45                    # paso angular
      cx  <- r * cos(ang); cy <- r * sin(ang)
      # colisión por cuadros (AABB) contra todas las ya colocadas
      libre <- TRUE
      if (length(cx0) > 0) {
        solapa <- (abs(cx - cx0) * 2 < (w[i] + cw)) & (abs(cy - cy0) * 2 < (h[i] + ch))
        if (any(solapa)) libre <- FALSE
      }
      if (libre || t > 4000) {
        px[i] <- cx; py[i] <- cy
        cx0 <- c(cx0, cx); cy0 <- c(cy0, cy); cw <- c(cw, w[i]); ch <- c(ch, h[i])
        break
      }
      t <- t + 1
    }
  }

  # rango de ejes ajustado a la extensión real (mantiene proporción px↔coords)
  ext_x <- max(abs(px) + w / 2) * 1.05
  ext_y <- max(abs(py) + h / 2) * 1.05
  col <- rep(PALETA_CGR, length.out = k)

  plot_ly(
    x = px, y = py, type = "scatter", mode = "text",
    text = d$palabra, textfont = list(size = size, color = col, family = "DM Sans, sans-serif"),
    hovertext = paste0(d$palabra, ": ", d$freq), hoverinfo = "text"
  ) |>
    layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE, range = c(-ext_x, ext_x)),
      yaxis = list(visible = FALSE, range = c(-ext_y, ext_y)),
      margin = list(t = 6, r = 6, b = 6, l = 6), showlegend = FALSE
    ) |>
    config(displayModeBar = FALSE)
}

metrica_box <- function(valor, etiqueta, acento = c("teal", "rosa", "navy")) {
  acento <- match.arg(acento)
  div(class = paste0("cgr-metrica acento-", acento),
      div(class = "valor", valor),
      div(class = "etiqueta", etiqueta))
}

# ---------------------------------------------------------------------------
# Carga de datos (real o sintético)
# ---------------------------------------------------------------------------
# Base URL del repo público (raw). La app desplegada (shinyapps.io) lee los
# datos directo de GitHub, así refleja siempre lo último que pusheó el bot
# diario sin necesidad de re-desplegar. En local se usan los archivos del disco.
CGR_DATA_URL <- Sys.getenv(
  "CGR_DATA_URL",
  "https://raw.githubusercontent.com/rrs111/cgr_prensa/main/datos"
)

# Lee un parquet/rds desde una ruta local o una URL (descarga a tempfile)
.leer_dato <- function(ruta, nombre) {
  es_url <- grepl("^https?://", ruta)
  if (es_url) {
    tmp <- tempfile(fileext = paste0(".", tools::file_ext(nombre)))
    ok <- tryCatch(
      utils::download.file(ruta, tmp, quiet = TRUE, mode = "wb") == 0,
      error = function(e) FALSE
    )
    if (!ok) return(NULL)
    ruta <- tmp
    on.exit(unlink(tmp), add = TRUE)
  }
  if (grepl("\\.parquet$", nombre)) {
    tryCatch(arrow::read_parquet(ruta), error = function(e) NULL)
  } else {
    tryCatch(readRDS(ruta), error = function(e) NULL)
  }
}

cargar_archivo <- function(nombre) {
  # 1) buscar local (desarrollo / ejecución desde el repo)
  cands <- c(file.path("datos", nombre),
             file.path("..", "datos", nombre),
             file.path("app", "datos", nombre))
  hit <- cands[file.exists(cands)]
  if (length(hit) > 0) return(.leer_dato(hit[1], nombre))

  # 2) si no hay local (app desplegada), leer del repo público en GitHub
  if (nzchar(CGR_DATA_URL)) {
    return(.leer_dato(paste0(CGR_DATA_URL, "/", nombre), nombre))
  }
  NULL
}

# --- generador de datos sintéticos coherentes -------------------------------
generar_sinteticos <- function() {
  set.seed(2026)
  fuentes <- c("Emol", "La Tercera", "La Segunda", "BioBioChile", "CIPER",
               "El Mostrador", "Cooperativa", "Diario Financiero", "Ex-Ante",
               "CNN Chile", "T13", "Pauta", "El Dínamo", "El Desconcierto",
               "Interferencia", "El Líbero", "The Clinic")
  vocab <- c("contraloria", "contralor", "dictamen", "fiscalizacion", "probidad",
             "transparencia", "auditoria", "sumario", "corrupcion", "irregularidad",
             "funcionario", "fondos", "municipio", "gasto", "presupuesto",
             "investigacion", "recursos", "alcalde", "ministerio", "rendicion",
             "patrimonio", "soborno", "malversacion", "dorothy", "perez")
  cats <- c("institucional", "funciones", "tematica",
            "institucional;funciones", "funciones;tematica")

  semanas <- floor_date(today(), "week", week_start = 1) - weeks(39:0)
  n <- 520
  datos <- tibble(
    id = sprintf("syn%04d", 1:n),
    fuente = sample(fuentes, n, replace = TRUE,
                    prob = c(8,7,4,7,5,8,6,3,3,5,5,4,4,4,4,4,4)),
    semana = sample(semanas, n, replace = TRUE,
                    prob = seq(0.4, 1.6, length.out = length(semanas))),
    palabra_clave = sample(vocab, n, replace = TRUE)
  ) |>
    mutate(
      fecha = semana + days(sample(0:6, n, replace = TRUE)),
      anio = year(fecha),
      categorias = sample(cats, n, replace = TRUE),
      titulo = str_to_sentence(paste(
        sample(c("Contraloría", "Contralor", "Informe", "Auditoría", "Investigación",
                 "Dictamen", "Fiscalización"), n, replace = TRUE),
        sample(c("detecta", "investiga", "cuestiona", "advierte sobre", "revela",
                 "objeta", "ordena revisar"), n, replace = TRUE),
        sample(c("uso de fondos públicos", "probidad en municipio", "gasto en ministerio",
                 "irregularidades en licitación", "rendición de cuentas",
                 "contrato observado", "sumario administrativo"), n, replace = TRUE))),
      bajada = paste("Análisis sobre", palabra_clave, "y el rol de la Contraloría General de la República."),
      url = paste0("https://ejemplo.cl/noticia/", id)
    ) |>
    select(id, fuente, fecha, semana, anio, titulo, bajada, categorias, url)

  # tokens semanales sintéticos
  palabras_semana <- datos |>
    select(semana, fuente) |>
    crossing(palabra = vocab) |>
    mutate(n = rpois(n(), lambda = runif(n(), 0.4, 4))) |>
    filter(n > 0)

  noticias_semana <- datos |> count(semana, fuente, name = "n_noticias")

  # TF-IDF calculado a mano (evita depender de {tidytext} en el bundle de deploy)
  tfidf_fuente <- palabras_semana |>
    count(fuente, palabra, wt = n, name = "n") |>
    group_by(fuente) |> mutate(tf = n / sum(n)) |> ungroup() |>
    add_count(palabra, name = "docs_term") |>
    mutate(idf = log(n_distinct(fuente) / docs_term),
           tf_idf = tf * idf) |>
    select(fuente, palabra, n, tf, idf, tf_idf)

  # tono sintético
  tono_articulo <- datos |>
    transmute(id, fuente, semana,
              clase = sample(c("negativo", "neutro", "positivo"), n(), TRUE, c(.2, .5, .3))) |>
    mutate(score = case_match(clase,
              "negativo" ~ runif(n(), -.6, -.2),
              "positivo" ~ runif(n(), .2, .6),
              .default = runif(n(), -.15, .15)),
           n_lex = sample(3:20, n(), TRUE))
  tono_semana <- tono_articulo |> group_by(semana) |>
    summarise(n_noticias = n(), score_prom = round(mean(score), 3),
              pct_neg = round(mean(clase == "negativo"), 3),
              pct_neu = round(mean(clase == "neutro"), 3),
              pct_pos = round(mean(clase == "positivo"), 3), .groups = "drop") |>
    mutate(indice_presion = round(n_noticias * pct_neg)) |> arrange(semana)
  tono_fuente <- tono_articulo |> group_by(fuente) |>
    summarise(n = n(), score_prom = round(mean(score), 3),
              pct_neg = round(mean(clase == "negativo"), 3),
              pct_pos = round(mean(clase == "positivo"), 3), .groups = "drop") |>
    arrange(score_prom)

  metricas <- list(
    total = nrow(datos),
    semana_actual = sum(datos$semana == max(semanas)),
    fuentes_activas = n_distinct(datos$fuente),
    ultima_fecha = max(datos$fecha),
    actualizado = now()
  )

  # temas sintéticos: 7 etiquetas plausibles del dominio + gamma Dirichlet por doc
  set.seed(2027)
  K_syn <- 7L
  etiq_syn <- c(
    "corrupcion / municipio / fondos",
    "auditoria / contrato / licitacion",
    "dictamen / decreto / toma de razon",
    "sumario / funcionarios / responsabilidad",
    "transparencia / probidad / declaracion",
    "alcalde / gobierno regional / gore",
    "presupuesto / gasto / ministerio"
  )
  etiquetas_df <- tibble::tibble(tema = 1:K_syn, etiqueta = etiq_syn)

  # Dirichlet por documento: rgamma con shape pequeño da distribuciones picudas
  gamma_mat <- matrix(rgamma(nrow(datos) * K_syn, shape = 0.5), nrow = nrow(datos))
  gamma_mat <- gamma_mat / rowSums(gamma_mat)
  colnames(gamma_mat) <- paste0("T", 1:K_syn)
  temas_doc <- bind_cols(datos |> select(id, fuente, semana, fecha),
                         as.data.frame(gamma_mat)) |>
    tidyr::pivot_longer(starts_with("T"), names_to = "tema_str", values_to = "gamma") |>
    mutate(tema = as.integer(sub("T", "", tema_str))) |>
    select(-tema_str) |>
    left_join(etiquetas_df, by = "tema")

  # top términos por tema = muestrear del vocab
  temas_terminos <- map_dfr(1:K_syn, function(k) {
    pal <- sample(vocab, 10)
    tibble(tema = k, etiqueta = etiq_syn[k], palabra = pal,
           beta = sort(runif(10, 0.01, 0.08), decreasing = TRUE))
  })

  temas_semana <- temas_doc |>
    group_by(semana, tema, etiqueta) |>
    summarise(prevalencia = mean(gamma), n_doc = n_distinct(id), .groups = "drop") |>
    arrange(semana, tema)

  # entidades sintéticas: actores plausibles del dominio CGR
  ent_plantilla <- list(
    persona = c("Dorothy Pérez", "Gabriel Boric", "José Antonio Kast",
                "Mario Desbordes", "Carolina Tohá", "Evelyn Matthei",
                "Pamela Jiles", "Daniel Jadue", "Karol Cariola"),
    organizacion = c("Ministerio de Hacienda", "Ministerio del Interior",
                     "Corte Suprema", "Servicio de Impuestos Internos",
                     "Carabineros", "Fiscalía Nacional", "Banco Central",
                     "Cámara de Diputados", "Senado"),
    lugar = c("Santiago", "Valparaíso", "Concepción", "Antofagasta",
              "Las Condes", "Maipú", "Metropolitana", "Biobío"),
    cargo = c("ministro", "alcalde", "gobernador", "diputado",
              "presidente", "fiscal", "senador", "director")
  )
  ent_filas <- map2_dfr(names(ent_plantilla), ent_plantilla, function(tp, lst) {
    map_dfr(lst, function(e) {
      ids <- sample(datos$id, sample(3:25, 1))
      tibble(id = ids, entidad = e, tipo = tp)
    })
  })
  entidades <- ent_filas
  entidades_resumen <- entidades |>
    left_join(datos |> select(id, fuente), by = "id") |>
    group_by(entidad, tipo) |>
    summarise(n_menciones = n_distinct(id),
              n_fuentes = n_distinct(fuente), .groups = "drop") |>
    arrange(desc(n_menciones))
  entidades_semana <- entidades |>
    left_join(datos |> select(id, semana), by = "id") |>
    group_by(semana, entidad, tipo) |>
    summarise(n = n_distinct(id), .groups = "drop") |>
    arrange(semana, desc(n))

  # red de co-ocurrencia sintética: 30 palabras en círculo + aristas aleatorias
  red_palabras <- sample(vocab, min(30, length(vocab)))
  ang <- seq(0, 2 * pi, length.out = length(red_palabras) + 1)[-1]
  red_nodos <- tibble(
    palabra = red_palabras,
    x = cos(ang) + rnorm(length(ang), sd = 0.08),
    y = sin(ang) + rnorm(length(ang), sd = 0.08),
    peso = sample(50:300, length(red_palabras), replace = TRUE)
  )
  combos <- expand.grid(palabra_a = red_palabras, palabra_b = red_palabras,
                        stringsAsFactors = FALSE) |>
    filter(palabra_a < palabra_b)
  red_aristas <- combos |>
    sample_n(60) |>
    mutate(peso = sample(5:50, 60, replace = TRUE)) |>
    left_join(red_nodos |> select(palabra, x0 = x, y0 = y), by = c("palabra_a" = "palabra")) |>
    left_join(red_nodos |> select(palabra, x1 = x, y1 = y), by = c("palabra_b" = "palabra"))

  # postura LLM sintética: distribución distinta a la del tono léxico, para que
  # en demo se note que son métricas diferentes (más desfavorables)
  postura_articulo <- datos |>
    transmute(id, postura = sample(c("desfavorable", "neutra", "favorable"),
                                   n(), TRUE, c(.35, .4, .25)))
  pa_syn <- postura_articulo |> left_join(datos |> select(id, fuente, semana), by = "id")
  postura_semana <- pa_syn |> group_by(semana) |>
    summarise(n = n(), pct_desfavorable = round(mean(postura == "desfavorable"), 3),
              pct_neutra = round(mean(postura == "neutra"), 3),
              pct_favorable = round(mean(postura == "favorable"), 3), .groups = "drop") |>
    arrange(semana)
  postura_fuente <- pa_syn |> group_by(fuente) |>
    summarise(n = n(), pct_desfavorable = round(mean(postura == "desfavorable"), 3),
              pct_favorable = round(mean(postura == "favorable"), 3), .groups = "drop") |>
    arrange(desc(pct_desfavorable))

  list(datos = datos, palabras_semana = palabras_semana,
       noticias_semana = noticias_semana, tfidf_fuente = tfidf_fuente,
       tono_semana = tono_semana, tono_fuente = tono_fuente, tono_articulo = tono_articulo,
       postura_articulo = postura_articulo, postura_semana = postura_semana, postura_fuente = postura_fuente,
       temas_terminos = temas_terminos, temas_doc = temas_doc, temas_semana = temas_semana,
       entidades = entidades, entidades_resumen = entidades_resumen,
       entidades_semana = entidades_semana,
       red_nodos = red_nodos, red_aristas = red_aristas,
       metricas = metricas, sintetico = TRUE)
}

# --- cargar todo ------------------------------------------------------------
cargar_todo <- function() {
  datos <- cargar_archivo("cgr_datos.parquet")
  palabras_semana <- cargar_archivo("cgr_palabras_semana.parquet")
  if (is.null(datos) || is.null(palabras_semana) || nrow(datos) < 5) {
    message("Datos reales insuficientes: usando datos sintéticos de demostración.")
    return(generar_sinteticos())
  }
  noticias_semana <- cargar_archivo("cgr_noticias_semana.parquet")
  if (is.null(noticias_semana)) noticias_semana <- count(datos, semana, fuente, name = "n_noticias")
  tfidf_fuente <- cargar_archivo("cgr_tfidf_fuente.parquet")
  if (is.null(tfidf_fuente)) tfidf_fuente <- tibble(fuente = character(), palabra = character(), n = integer(), tf_idf = numeric())
  metricas <- cargar_archivo("cgr_metricas.rds")
  if (is.null(metricas)) metricas <- list(
    total = nrow(datos),
    semana_actual = sum(datos$semana == floor_date(today(), "week", week_start = 1), na.rm = TRUE),
    fuentes_activas = n_distinct(datos$fuente),
    ultima_fecha = max(datos$fecha, na.rm = TRUE), actualizado = now())

  tono_semana <- cargar_archivo("cgr_tono_semana.parquet")
  tono_fuente <- cargar_archivo("cgr_tono_fuente.parquet")
  tono_articulo <- cargar_archivo("cgr_tono_articulo.parquet")
  if (is.null(tono_articulo)) tono_articulo <- tibble(id = datos$id, score = 0, n_lex = 0L, clase = "neutro")
  if (is.null(tono_semana)) tono_semana <- tibble(semana = as.Date(character()), n_noticias = integer(),
       score_prom = numeric(), pct_neg = numeric(), pct_neu = numeric(), pct_pos = numeric(), indice_presion = integer())
  if (is.null(tono_fuente)) tono_fuente <- tibble(fuente = character(), n = integer(),
       score_prom = numeric(), pct_neg = numeric(), pct_pos = numeric())

  temas_terminos <- cargar_archivo("cgr_temas_terminos.parquet")
  temas_doc      <- cargar_archivo("cgr_temas_doc.parquet")
  temas_semana   <- cargar_archivo("cgr_temas_semana.parquet")
  if (is.null(temas_terminos)) temas_terminos <- tibble(tema = integer(), etiqueta = character(),
       palabra = character(), beta = numeric())
  if (is.null(temas_doc))      temas_doc <- tibble(id = character(), fuente = character(),
       semana = as.Date(character()), fecha = as.Date(character()),
       tema = integer(), etiqueta = character(), gamma = numeric())
  if (is.null(temas_semana))   temas_semana <- tibble(semana = as.Date(character()),
       tema = integer(), etiqueta = character(), prevalencia = numeric(), n_doc = integer())

  entidades         <- cargar_archivo("cgr_entidades.parquet")
  entidades_resumen <- cargar_archivo("cgr_entidades_resumen.parquet")
  entidades_semana  <- cargar_archivo("cgr_entidades_semana.parquet")
  if (is.null(entidades))         entidades <- tibble(id = character(), entidad = character(), tipo = character())
  if (is.null(entidades_resumen)) entidades_resumen <- tibble(entidad = character(),
       tipo = character(), n_menciones = integer(), n_fuentes = integer())
  if (is.null(entidades_semana))  entidades_semana <- tibble(semana = as.Date(character()),
       entidad = character(), tipo = character(), n = integer())

  red_nodos   <- cargar_archivo("cgr_red_nodos.parquet")
  red_aristas <- cargar_archivo("cgr_red_aristas.parquet")
  if (is.null(red_nodos))   red_nodos   <- tibble(palabra = character(), x = numeric(),
                                                  y = numeric(), peso = numeric())
  if (is.null(red_aristas)) red_aristas <- tibble(palabra_a = character(), palabra_b = character(),
                                                  peso = numeric(), x0 = numeric(), y0 = numeric(),
                                                  x1 = numeric(), y1 = numeric())

  # Postura hacia la CGR (LLM) — métrica opcional generada localmente
  postura_articulo <- cargar_archivo("cgr_postura_articulo.parquet")
  postura_semana   <- cargar_archivo("cgr_postura_semana.parquet")
  postura_fuente   <- cargar_archivo("cgr_postura_fuente.parquet")
  if (is.null(postura_articulo)) postura_articulo <- tibble(id = character(), postura = character())
  if (is.null(postura_semana))   postura_semana <- tibble(semana = as.Date(character()), n = integer(),
       pct_desfavorable = numeric(), pct_neutra = numeric(), pct_favorable = numeric())
  if (is.null(postura_fuente))   postura_fuente <- tibble(fuente = character(), n = integer(),
       pct_desfavorable = numeric(), pct_favorable = numeric())

  list(datos = datos, palabras_semana = palabras_semana,
       noticias_semana = noticias_semana, tfidf_fuente = tfidf_fuente,
       tono_semana = tono_semana, tono_fuente = tono_fuente, tono_articulo = tono_articulo,
       postura_articulo = postura_articulo, postura_semana = postura_semana, postura_fuente = postura_fuente,
       temas_terminos = temas_terminos, temas_doc = temas_doc, temas_semana = temas_semana,
       entidades = entidades, entidades_resumen = entidades_resumen,
       entidades_semana = entidades_semana,
       red_nodos = red_nodos, red_aristas = red_aristas,
       metricas = metricas, sintetico = FALSE)
}

D <- cargar_todo()

# vectores auxiliares
fuentes_disp <- sort(unique(D$datos$fuente))
top_terminos <- D$palabras_semana |>
  count(palabra, wt = n, sort = TRUE, name = "n") |>
  pull(palabra)
rango_fechas <- range(D$datos$fecha, na.rm = TRUE)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
tema <- bs_theme(
  version = 5,
  bg = COL_CREMA, fg = COL_NAVY,
  primary = COL_NAVY, secondary = COL_TEAL,
  base_font = font_google("DM Sans"),
  heading_font = font_google("DM Serif Display")
)

ui <- page_navbar(
  title = "CGR Prensa",
  id = "nav",
  theme = tema,
  lang = "es",
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "cgr_styles.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&family=DM+Serif+Display&display=swap")
  ),

  # ---- 1. RESUMEN ----
  nav_panel(
    "Resumen", icon = icon("chart-line"),
    div(class = "container-fluid",
      uiOutput("metricas_ui"),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Noticias sobre la CGR por semana"),
             withSpinner(plotlyOutput("g_semanas", height = 320), proxy.height = "320px")),
        card(card_header("Distribución por fuente"),
             withSpinner(plotlyOutput("g_fuentes", height = 320)))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Palabras más frecuentes"),
             withSpinner(plotlyOutput("g_top_palabras", height = 360))),
        card(card_header("Nube de palabras"),
             withSpinner(plotlyOutput("g_nube", height = 360)))
      )
    )
  ),

  # ---- 2. TONO ----
  nav_panel(
    "Tono", icon = icon("face-meh"),
    div(class = "container-fluid",
      uiOutput("tono_metricas_ui"),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Evolución del tono de la cobertura"),
             withSpinner(plotlyOutput("t_tono_semana", height = 320))),
        card(card_header("Índice de presión mediática (noticias negativas/semana)"),
             withSpinner(plotlyOutput("t_presion", height = 320)))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Tono por medio (de más crítico a más favorable)"),
             withSpinner(plotlyOutput("t_tono_fuente", height = 420))),
        card(card_header("Composición del tono por semana"),
             withSpinner(plotlyOutput("t_composicion", height = 420)))
      ),
      div(class = "cgr-footer", style = "text-align:left;",
          "Nota: mide el tono del lenguaje de la cobertura (léxico de polaridad en ",
          "español), no el sentimiento explícito hacia la CGR. Una noticia donde la ",
          "CGR detecta corrupción tiende a tono negativo por el hecho que reporta."),

      # --- Postura hacia la CGR (LLM) ---
      tags$hr(),
      tags$h4("Postura hacia la CGR (análisis con IA)", class = "cgr-titulo",
              style = "margin-top:10px;"),
      uiOutput("postura_metricas_ui"),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Composición de la postura por semana"),
             withSpinner(plotlyOutput("p_composicion", height = 320))),
        card(card_header("Postura por medio (% desfavorable a la CGR)"),
             withSpinner(plotlyOutput("p_fuente", height = 320)))
      ),
      div(class = "cgr-footer", style = "text-align:left;",
          "A diferencia del tono por léxico, esta métrica usa un modelo de lenguaje ",
          "(Llama local, vía Ollama) que LEE cada noticia y juzga si deja a la ",
          "Contraloría favorable / neutra / desfavorable, entendiendo el contexto ",
          "(p. ej. una nota sobre cuestionamientos a la contralora es desfavorable, ",
          "aunque su lenguaje sea formal). Se genera localmente con ",
          tags$code("Rscript procesamiento/cgr_sentimiento_llm.R"), ".")
    )
  ),

  # ---- 3. TEMAS ----
  nav_panel(
    "Temas", icon = icon("layer-group"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        selectInput("temas_seleccion", "Tema a inspeccionar:",
                    choices = NULL, selected = NULL)
      ),
      uiOutput("temas_metricas_ui"),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Top términos del tema seleccionado"),
             withSpinner(plotlyOutput("temas_terminos_plot", height = 380))),
        card(card_header("Prevalencia de los temas a lo largo del tiempo"),
             withSpinner(plotlyOutput("temas_evolucion", height = 380)))
      ),
      card(card_header("Artículos más representativos del tema seleccionado"),
           withSpinner(DTOutput("temas_docs_tabla"))),
      div(class = "cgr-footer", style = "text-align:left;",
          "Modelado con {stm} (K = 8 temas). La etiqueta de cada tema es ",
          "provisional (top 3 términos por β); se puede sustituir por una ",
          "descripción legible en la Fase 3 (LLM).")
    )
  ),

  # ---- 4. ACTORES ----
  nav_panel(
    "Actores", icon = icon("users"),
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        pickerInput("act_tipos", "Tipo de actor:",
                    choices = c("persona", "organizacion", "lugar", "cargo"),
                    selected = c("persona", "organizacion"),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE,
                                   `selected-text-format` = "count > 2")),
        sliderInput("act_top_n", "Cuántos mostrar:", min = 10, max = 50, value = 20, step = 5),
        textInput("act_buscar", "Buscar actor:", placeholder = "ej: Boric, ministerio")
      ),
      uiOutput("act_metricas_ui"),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Actores más mencionados"),
             withSpinner(plotlyOutput("act_top", height = 460))),
        card(card_header("Evolución temporal de los top 5 actores"),
             withSpinner(plotlyOutput("act_evolucion", height = 460)))
      ),
      card(card_header("Detalle de actores"),
           withSpinner(DTOutput("act_tabla"))),
      div(class = "cgr-footer", style = "text-align:left;",
          "NER vía {udpipe} (modelo spanish-gsd) sobre título + bajada. ",
          "Personas, organizaciones, lugares y cargos. Los nombres de los ",
          "medios y la propia CGR se excluyen del listado por ser sujetos, ",
          "no actores de la noticia. La precisión es razonable sin LLM; ",
          "puede haber alguna mala clasificación (p. ej. apellidos sueltos).")
    )
  ),

  # ---- 5. TENDENCIAS ----
  nav_panel(
    "Tendencias", icon = icon("arrow-trend-up"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput("t_terminos", "Términos (hasta 5):",
                       choices = head(top_terminos, 200),
                       selected = head(top_terminos, 3),
                       multiple = TRUE, options = list(maxItems = 5)),
        pickerInput("t_fuentes", "Fuentes:", choices = fuentes_disp,
                    selected = fuentes_disp, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2")),
        sliderInput("t_fechas", "Rango de fechas:",
                    min = rango_fechas[1], max = rango_fechas[2],
                    value = rango_fechas, timeFormat = "%b %Y")
      ),
      card(card_header("Evolución temporal de términos"),
           withSpinner(plotlyOutput("g_tendencia", height = 380))),
      card(card_header("Palabras emergentes (últimas 2 semanas vs. anteriores)"),
           withSpinner(plotlyOutput("g_emergentes", height = 320))),
      card(card_header("Red de co-ocurrencia (qué palabras aparecen juntas)"),
           withSpinner(plotlyOutput("g_red", height = 520)),
           div(class = "cgr-footer", style = "text-align:left;",
               "Top 50 términos por fuerza de co-ocurrencia (ventana de 5 ",
               "palabras). Layout Fruchterman-Reingold. Tamaño del nodo y grosor ",
               "de la arista proporcionales al peso."))
    )
  ),

  # ---- 6. FUENTES ----
  nav_panel(
    "Fuentes", icon = icon("newspaper"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        pickerInput("f_fuentes", "Comparar fuentes:", choices = fuentes_disp,
                    selected = head(fuentes_disp, 6), multiple = TRUE,
                    options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Noticias por fuente"),
             withSpinner(plotlyOutput("f_conteo", height = 340))),
        card(card_header("Palabras más frecuentes por fuente"),
             withSpinner(plotlyOutput("f_palabras", height = 340)))
      ),
      card(card_header("Términos distintivos por fuente (TF-IDF)"),
           withSpinner(DTOutput("f_tfidf")))
    )
  ),

  # ---- 7. NOTICIAS ----
  nav_panel(
    "Noticias", icon = icon("magnifying-glass"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        textInput("n_busqueda", "Buscar en el título:", placeholder = "ej: probidad"),
        pickerInput("n_fuentes", "Fuentes:", choices = fuentes_disp,
                    selected = fuentes_disp, multiple = TRUE,
                    options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2")),
        sliderInput("n_fechas", "Rango de fechas:",
                    min = rango_fechas[1], max = rango_fechas[2],
                    value = rango_fechas, timeFormat = "%d %b %Y"),
        hr(),
        textInput("kwic_termino", "Palabra en contexto (KWIC):",
                  placeholder = "ej: contraloria, licencia, sumario")
      ),
      card(card_header(textOutput("n_titulo_tabla")),
           withSpinner(DTOutput("n_tabla"))),
      card(card_header("Palabra en contexto (KWIC)"),
           withSpinner(DTOutput("kwic_tabla")),
           div(class = "cgr-footer", style = "text-align:left;",
               "Frases reales del cuerpo de las noticias donde aparece el ",
               "término. Útil para entender el sentido y el tono del uso."))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text", style = "color:#74CEC4;font-size:.8rem;",
                     textOutput("estado_datos", inline = TRUE))),

  footer = div(class = "cgr-footer",
    "CGR Prensa · Monitor de cobertura de prensa sobre la Contraloría General de la República · ",
    "Arquitectura basada en ", tags$a(href = "https://github.com/bastianolea/prensa_chile",
                                       "prensa_chile"), " de Bastián Olea.")
)

# ---------------------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # estado de los datos (real / demo)
  output$estado_datos <- renderText({
    if (isTRUE(D$sintetico)) "● datos de demostración" else "● datos reales"
  })

  # ===== RESUMEN =====
  output$metricas_ui <- renderUI({
    m <- D$metricas
    fmt <- function(x) formatC(as.integer(x), big.mark = ".", format = "d")
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      metrica_box(fmt(m$total), "Noticias CGR", "navy"),
      metrica_box(fmt(m$semana_actual), "Esta semana", "rosa"),
      metrica_box(m$fuentes_activas, "Fuentes activas", "teal"),
      metrica_box(format(as.Date(m$ultima_fecha), "%d %b %Y"), "Última noticia", "teal")
    )
  })

  output$g_semanas <- renderPlotly({
    d <- D$noticias_semana |>
      group_by(semana) |>
      summarise(n = sum(n_noticias), .groups = "drop") |>
      arrange(semana)
    plot_ly(d, x = ~semana, y = ~n, type = "scatter", mode = "lines",
            fill = "tozeroy", line = list(color = COL_NAVY, width = 2.5),
            fillcolor = "rgba(116,206,196,0.35)",
            hovertemplate = "Semana del %{x|%d %b %Y}<br>%{y} noticias<extra></extra>") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Noticias"))
  })

  output$g_fuentes <- renderPlotly({
    d <- D$datos |> count(fuente, sort = TRUE, name = "n") |> head(12) |>
      mutate(fuente = factor(fuente, levels = rev(fuente)))
    plot_ly(d, x = ~n, y = ~fuente, type = "bar", orientation = "h",
            marker = list(color = COL_TEAL, line = list(color = COL_NAVY, width = 1)),
            hovertemplate = "%{y}: %{x} noticias<extra></extra>") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = "Noticias"), yaxis = list(title = ""))
  })

  output$g_top_palabras <- renderPlotly({
    d <- D$palabras_semana |> count(palabra, wt = n, sort = TRUE, name = "n") |>
      head(15) |> mutate(palabra = factor(palabra, levels = rev(palabra)))
    plot_ly(d, x = ~n, y = ~palabra, type = "bar", orientation = "h",
            marker = list(color = COL_NAVY),
            hovertemplate = "%{y}: %{x}<extra></extra>") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = "Frecuencia"), yaxis = list(title = ""))
  })

  output$g_nube <- renderPlotly({
    d <- D$palabras_semana |> count(palabra, wt = n, sort = TRUE, name = "freq")
    validate(need(nrow(d) > 0, "Sin datos"))
    nube_plotly(d, n_palabras = 70)
  })

  # ===== TONO =====
  output$tono_metricas_ui <- renderUI({
    ta <- D$tono_articulo
    if (is.null(ta) || nrow(ta) == 0) {
      return(div(class = "cgr-footer", "Sin datos de tono — corre `Rscript cgr_procesar.R`."))
    }
    pct_neg <- mean(ta$clase == "negativo")
    pct_pos <- mean(ta$clase == "positivo")
    score <- mean(ta$score)
    etiqueta <- if (score <= -0.1) "Tono crítico" else if (score >= 0.1) "Tono favorable" else "Tono neutral"
    ult <- if (nrow(D$tono_semana) > 0) tail(D$tono_semana$indice_presion, 1) else 0
    fmt <- function(x) paste0(formatC(100 * x, digits = 1, format = "f"), "%")
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      metrica_box(etiqueta, paste0("Score promedio ", round(score, 2)), "navy"),
      metrica_box(fmt(pct_neg), "% noticias negativas", "rosa"),
      metrica_box(fmt(pct_pos), "% noticias positivas", "teal"),
      metrica_box(formatC(as.integer(ult), big.mark = ".", format = "d"),
                  "Presión última semana", "rosa")
    )
  })

  output$t_tono_semana <- renderPlotly({
    d <- D$tono_semana |> arrange(semana)
    validate(need(nrow(d) > 0, "Sin datos de tono semanal"))
    plot_ly(d, x = ~semana, y = ~score_prom, type = "scatter", mode = "lines+markers",
            line = list(color = COL_NAVY, width = 2.5),
            marker = list(size = 5, color = COL_NAVY),
            hovertemplate = "%{x|%d %b %Y}<br>Score: %{y:.2f}<extra></extra>") |>
      add_lines(y = 0, line = list(color = "rgba(27,31,73,0.25)", dash = "dot"),
                showlegend = FALSE, hoverinfo = "skip") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Score de tono (-1 crítico · +1 favorable)",
                          range = c(-0.6, 0.6)))
  })

  output$t_presion <- renderPlotly({
    d <- D$tono_semana |> arrange(semana)
    validate(need(nrow(d) > 0, "Sin datos"))
    plot_ly(d, x = ~semana, y = ~indice_presion, type = "bar",
            marker = list(color = COL_ROSA),
            hovertemplate = "%{x|%d %b %Y}<br>%{y} noticias negativas<extra></extra>") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Noticias negativas"))
  })

  output$t_tono_fuente <- renderPlotly({
    d <- D$tono_fuente |> filter(n >= 5) |> arrange(score_prom)
    validate(need(nrow(d) > 0, "Sin datos por fuente"))
    d <- d |> mutate(fuente = factor(fuente, levels = fuente),
                     color = ifelse(score_prom < 0, COL_ROSA, COL_TEAL))
    plot_ly(d, x = ~score_prom, y = ~fuente, type = "bar", orientation = "h",
            marker = list(color = ~color, line = list(color = COL_NAVY, width = 0.5)),
            text = ~paste0("n=", n), textposition = "outside",
            hovertemplate = "%{y}<br>Score: %{x:.2f}<br>%{text}<extra></extra>") |>
      add_lines(x = 0, y = ~fuente, line = list(color = "rgba(27,31,73,0.3)", dash = "dot"),
                showlegend = FALSE, hoverinfo = "skip", inherit = FALSE) |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = "Score promedio"), yaxis = list(title = ""))
  })

  output$t_composicion <- renderPlotly({
    d <- D$tono_semana |> arrange(semana)
    validate(need(nrow(d) > 0, "Sin datos"))
    plot_ly(d, x = ~semana) |>
      add_trace(y = ~pct_neg, name = "Negativo", type = "scatter", mode = "none",
                stackgroup = "uno", fillcolor = COL_ROSA,
                hovertemplate = "Negativo: %{y:.0%}<extra></extra>") |>
      add_trace(y = ~pct_neu, name = "Neutro", type = "scatter", mode = "none",
                stackgroup = "uno", fillcolor = "rgba(27,31,73,0.55)",
                hovertemplate = "Neutro: %{y:.0%}<extra></extra>") |>
      add_trace(y = ~pct_pos, name = "Positivo", type = "scatter", mode = "none",
                stackgroup = "uno", fillcolor = COL_TEAL,
                hovertemplate = "Positivo: %{y:.0%}<extra></extra>") |>
      estilo_plotly() |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Composición", tickformat = ".0%", range = c(0, 1)))
  })

  # ----- Postura hacia la CGR (LLM) -----
  hay_postura <- reactive({
    nrow(D$postura_articulo) > 0 && any(!is.na(D$postura_articulo$postura))
  })

  output$postura_metricas_ui <- renderUI({
    if (!hay_postura()) {
      return(div(class = "cgr-footer", style = "text-align:left;",
                 "Aún no hay análisis de postura. Generalo localmente con ",
                 tags$code("Rscript procesamiento/cgr_sentimiento_llm.R"),
                 " (requiere Ollama)."))
    }
    pa <- D$postura_articulo
    pct <- function(c) paste0(round(100 * mean(pa$postura == c)), "%")
    fmt_ult <- if (nrow(D$postura_semana) > 0) {
      paste0(round(100 * tail(D$postura_semana$pct_desfavorable, 1)), "%")
    } else "—"
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      metrica_box(pct("desfavorable"), "Desfavorable a la CGR", "rosa"),
      metrica_box(pct("neutra"), "Neutra", "navy"),
      metrica_box(pct("favorable"), "Favorable", "teal"),
      metrica_box(fmt_ult, "Desfavorable última semana", "rosa")
    )
  })

  output$p_composicion <- renderPlotly({
    validate(need(hay_postura() && nrow(D$postura_semana) > 0,
                  "Sin datos de postura — corré el script LLM local."))
    d <- D$postura_semana |> arrange(semana)
    plot_ly(d, x = ~semana) |>
      add_trace(y = ~pct_desfavorable, name = "Desfavorable", type = "scatter", mode = "none",
                stackgroup = "uno", fillcolor = COL_ROSA,
                hovertemplate = "Desfavorable: %{y:.0%}<extra></extra>") |>
      add_trace(y = ~pct_neutra, name = "Neutra", type = "scatter", mode = "none",
                stackgroup = "uno", fillcolor = "rgba(27,31,73,0.55)",
                hovertemplate = "Neutra: %{y:.0%}<extra></extra>") |>
      add_trace(y = ~pct_favorable, name = "Favorable", type = "scatter", mode = "none",
                stackgroup = "uno", fillcolor = COL_TEAL,
                hovertemplate = "Favorable: %{y:.0%}<extra></extra>") |>
      estilo_plotly() |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Composición", tickformat = ".0%", range = c(0, 1)))
  })

  output$p_fuente <- renderPlotly({
    validate(need(hay_postura() && nrow(D$postura_fuente) > 0,
                  "Sin datos de postura por fuente."))
    d <- D$postura_fuente |> filter(n >= 3) |> arrange(pct_desfavorable) |>
      mutate(fuente = factor(fuente, levels = fuente))
    plot_ly(d, x = ~pct_desfavorable, y = ~fuente, type = "bar", orientation = "h",
            marker = list(color = COL_ROSA, line = list(color = COL_NAVY, width = 0.5)),
            text = ~paste0("n=", n), textposition = "outside",
            hovertemplate = "%{y}<br>%{x:.0%} desfavorable<br>%{text}<extra></extra>") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = "% noticias desfavorables", tickformat = ".0%"),
             yaxis = list(title = ""))
  })

  # ===== TEMAS =====
  # Etiquetas (mapa tema -> etiqueta) y choices del selector
  temas_choices <- reactive({
    if (nrow(D$temas_terminos) == 0) return(setNames(integer(), character()))
    et <- D$temas_terminos |> distinct(tema, etiqueta) |> arrange(tema)
    setNames(et$tema, paste0("T", et$tema, " — ", et$etiqueta))
  })

  observe({
    ch <- temas_choices()
    updateSelectInput(session, "temas_seleccion", choices = ch,
                      selected = if (length(ch)) ch[1] else character(0))
  })

  output$temas_metricas_ui <- renderUI({
    if (nrow(D$temas_semana) == 0) {
      return(div(class = "cgr-footer", "Sin datos de temas — corre `Rscript cgr_procesar.R`."))
    }
    n_temas <- n_distinct(D$temas_terminos$tema)
    ult <- max(D$temas_semana$semana, na.rm = TRUE)
    dom_ult <- D$temas_semana |> filter(semana == ult) |> slice_max(prevalencia, n = 1)
    cobertura <- if (nrow(dom_ult)) round(100 * dom_ult$prevalencia[1], 0) else 0
    layout_columns(
      col_widths = c(3, 6, 3),
      metrica_box(n_temas, "Temas detectados", "navy"),
      metrica_box(if (nrow(dom_ult)) dom_ult$etiqueta[1] else "—",
                  "Tema dominante última semana", "teal"),
      metrica_box(paste0(cobertura, "%"),
                  "Prevalencia del dominante", "rosa")
    )
  })

  output$temas_terminos_plot <- renderPlotly({
    req(input$temas_seleccion)
    tema_sel <- as.integer(input$temas_seleccion)
    d <- D$temas_terminos |> filter(tema == tema_sel) |>
      arrange(desc(beta)) |> head(12) |>
      mutate(palabra = factor(palabra, levels = rev(palabra)))
    validate(need(nrow(d) > 0, "Sin términos para este tema"))
    plot_ly(d, x = ~beta, y = ~palabra, type = "bar", orientation = "h",
            marker = list(color = COL_NAVY),
            hovertemplate = "%{y}: β=%{x:.3f}<extra></extra>") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = "β (probabilidad en el tema)"),
             yaxis = list(title = ""))
  })

  output$temas_evolucion <- renderPlotly({
    d <- D$temas_semana |> arrange(semana, tema)
    validate(need(nrow(d) > 0, "Sin datos de prevalencia"))
    # paleta extendida para hasta ~10 temas
    pal <- rep(PALETA_CGR, length.out = max(d$tema, na.rm = TRUE))
    d_split <- split(d, d$tema)
    p <- plot_ly()
    for (k in seq_along(d_split)) {
      dk <- d_split[[k]]
      lbl <- paste0("T", dk$tema[1], " — ", dk$etiqueta[1])
      p <- p |> add_trace(x = dk$semana, y = dk$prevalencia, name = lbl,
                          type = "scatter", mode = "none",
                          stackgroup = "uno", fillcolor = pal[dk$tema[1]],
                          hovertemplate = paste0(lbl, "<br>%{x|%d %b %Y}: %{y:.0%}<extra></extra>"))
    }
    p |> estilo_plotly() |>
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Prevalencia", tickformat = ".0%", range = c(0, 1)))
  })

  output$temas_docs_tabla <- renderDT({
    req(input$temas_seleccion)
    tema_sel <- as.integer(input$temas_seleccion)
    top_docs <- D$temas_doc |> filter(tema == tema_sel) |>
      arrange(desc(gamma)) |> head(50) |> select(id, gamma)
    d <- top_docs |>
      inner_join(D$datos |> select(id, fecha, fuente, titulo, url), by = "id") |>
      transmute(
        fecha = format(fecha, "%Y-%m-%d"),
        fuente = fuente,
        titulo = ifelse(is.na(url) | url == "" | startsWith(url, "muestra2025"),
                        htmltools::htmlEscape(titulo),
                        paste0("<a href='", url, "' target='_blank'>",
                               htmltools::htmlEscape(titulo), "</a>")),
        peso = sprintf("%.0f%%", 100 * gamma)
      )
    datatable(d, rownames = FALSE, escape = FALSE,
              colnames = c("Fecha", "Fuente", "Título", "Peso en el tema"),
              options = list(pageLength = 12, order = list(list(3, "desc")),
                language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
  })

  # ===== ACTORES =====
  ent_filtradas <- reactive({
    d <- D$entidades_resumen |> filter(tipo %in% input$act_tipos)
    if (nzchar(input$act_buscar)) {
      d <- d |> filter(str_detect(str_to_lower(entidad),
                                  str_to_lower(input$act_buscar)))
    }
    d |> arrange(desc(n_menciones))
  })

  output$act_metricas_ui <- renderUI({
    d <- D$entidades_resumen
    if (nrow(d) == 0) {
      return(div(class = "cgr-footer", "Sin datos de actores — corre `Rscript cgr_procesar.R`."))
    }
    n_total <- sum(d$n_menciones)
    n_pers  <- sum(d$tipo == "persona")
    n_org   <- sum(d$tipo == "organizacion")
    n_lug   <- sum(d$tipo == "lugar")
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      metrica_box(formatC(nrow(d), big.mark = ".", format = "d"),
                  "Actores únicos", "navy"),
      metrica_box(formatC(n_pers, big.mark = ".", format = "d"),
                  "Personas", "rosa"),
      metrica_box(formatC(n_org, big.mark = ".", format = "d"),
                  "Organizaciones", "teal"),
      metrica_box(formatC(n_lug, big.mark = ".", format = "d"),
                  "Lugares", "teal")
    )
  })

  output$act_top <- renderPlotly({
    d <- ent_filtradas() |> head(input$act_top_n)
    validate(need(nrow(d) > 0, "Sin actores para los filtros seleccionados"))
    color_tipo <- c(persona = COL_NAVY, organizacion = COL_TEAL,
                    lugar = COL_ROSA, cargo = "#3E6E8C")
    d <- d |> mutate(color = color_tipo[tipo],
                     entidad = factor(entidad, levels = rev(entidad)))
    plot_ly(d, x = ~n_menciones, y = ~entidad, type = "bar", orientation = "h",
            color = ~tipo, colors = color_tipo,
            hovertemplate = "%{y}<br>%{x} menciones<br>%{customdata} fuentes<extra>%{fullData.name}</extra>",
            customdata = ~n_fuentes) |>
      estilo_plotly() |>
      layout(xaxis = list(title = "Menciones"),
             yaxis = list(title = ""),
             legend = list(orientation = "h", y = 1.05))
  })

  output$act_evolucion <- renderPlotly({
    top5 <- ent_filtradas() |> head(5) |> pull(entidad)
    validate(need(length(top5) > 0, "Sin datos para evolución"))
    d <- D$entidades_semana |> filter(entidad %in% top5) |>
      complete(semana = unique(D$entidades_semana$semana), entidad = top5, fill = list(n = 0)) |>
      arrange(semana)
    plot_ly(d, x = ~semana, y = ~n, color = ~entidad, colors = PALETA_CGR,
            type = "scatter", mode = "lines",
            line = list(width = 2),
            hovertemplate = "%{x|%d %b %Y}<br>%{y} menciones<extra>%{fullData.name}</extra>") |>
      estilo_plotly() |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Menciones"))
  })

  output$act_tabla <- renderDT({
    d <- ent_filtradas() |>
      transmute(entidad, tipo, n_menciones, n_fuentes)
    datatable(d, rownames = FALSE,
              colnames = c("Actor", "Tipo", "Menciones", "Fuentes que cubren"),
              options = list(pageLength = 15,
                language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
  })

  # ===== TENDENCIAS =====
  output$g_tendencia <- renderPlotly({
    req(input$t_terminos)
    d <- D$palabras_semana |>
      filter(palabra %in% input$t_terminos,
             fuente %in% input$t_fuentes,
             semana >= input$t_fechas[1], semana <= input$t_fechas[2]) |>
      group_by(semana, palabra) |>
      summarise(n = sum(n), .groups = "drop")
    validate(need(nrow(d) > 0, "Sin datos para los términos/filtros seleccionados"))
    plot_ly(d, x = ~semana, y = ~n, color = ~palabra, colors = PALETA_CGR,
            type = "scatter", mode = "lines+markers",
            line = list(width = 2.5), marker = list(size = 5),
            hovertemplate = "%{x|%d %b %Y}<br>%{y}<extra>%{fullData.name}</extra>") |>
      estilo_plotly() |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Menciones"))
  })

  output$g_emergentes <- renderPlotly({
    semanas <- sort(unique(D$palabras_semana$semana))
    validate(need(length(semanas) >= 3, "Se necesitan al menos 3 semanas de datos"))
    recientes <- tail(semanas, 2)
    previas <- head(semanas, length(semanas) - 2)
    base_prev <- D$palabras_semana |> filter(fuente %in% input$t_fuentes, semana %in% previas) |>
      group_by(palabra) |> summarise(antes = sum(n), .groups = "drop")
    base_rec <- D$palabras_semana |> filter(fuente %in% input$t_fuentes, semana %in% recientes) |>
      group_by(palabra) |> summarise(ahora = sum(n), .groups = "drop")
    d <- full_join(base_rec, base_prev, by = "palabra") |>
      mutate(antes = coalesce(antes, 0), ahora = coalesce(ahora, 0),
             cambio = ahora - antes) |>
      filter(ahora >= 2) |> arrange(desc(cambio)) |> head(12) |>
      mutate(palabra = factor(palabra, levels = rev(palabra)))
    validate(need(nrow(d) > 0, "Sin palabras emergentes"))
    plot_ly(d, x = ~cambio, y = ~palabra, type = "bar", orientation = "h",
            marker = list(color = ifelse(d$cambio >= 0, COL_ROSA, COL_TEAL)),
            hovertemplate = "%{y}: %{x:+d} menciones<extra></extra>") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = "Cambio en menciones"), yaxis = list(title = ""))
  })

  # ----- Red de co-ocurrencia (Tendencias) -----
  output$g_red <- renderPlotly({
    nodos <- D$red_nodos
    aristas <- D$red_aristas
    validate(need(nrow(nodos) > 0 && nrow(aristas) > 0,
                  "Sin datos de red — corre `Rscript cgr_procesar.R`"))
    # aristas como líneas con NA entre cada par (técnica estándar plotly)
    ex <- as.vector(rbind(aristas$x0, aristas$x1, NA))
    ey <- as.vector(rbind(aristas$y0, aristas$y1, NA))
    # tamaño nodos escalado y label
    s_max <- max(nodos$peso, na.rm = TRUE)
    nodos <- nodos |> mutate(size = 10 + 35 * peso / s_max)
    plot_ly() |>
      add_trace(x = ex, y = ey, type = "scatter", mode = "lines",
                line = list(color = "rgba(27,31,73,0.18)", width = 1.2),
                hoverinfo = "skip", showlegend = FALSE) |>
      add_trace(x = nodos$x, y = nodos$y, type = "scatter", mode = "markers+text",
                marker = list(size = nodos$size, color = COL_TEAL,
                              line = list(color = COL_NAVY, width = 1.5),
                              opacity = 0.85),
                text = nodos$palabra, textposition = "top center",
                textfont = list(family = "DM Sans, sans-serif",
                                size = 12, color = COL_NAVY),
                hovertemplate = paste0("<b>%{text}</b><br>peso: ",
                                       round(nodos$peso, 0), "<extra></extra>"),
                showlegend = FALSE) |>
      estilo_plotly(leyenda = FALSE) |>
      layout(
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE,
                     showticklabels = FALSE),
        yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE,
                     showticklabels = FALSE)
      )
  })

  # ===== FUENTES =====
  output$f_conteo <- renderPlotly({
    req(input$f_fuentes)
    d <- D$datos |> filter(fuente %in% input$f_fuentes) |>
      count(fuente, sort = TRUE, name = "n") |>
      mutate(fuente = factor(fuente, levels = rev(fuente)))
    validate(need(nrow(d) > 0, "Selecciona al menos una fuente"))
    plot_ly(d, x = ~n, y = ~fuente, type = "bar", orientation = "h",
            marker = list(color = COL_NAVY),
            hovertemplate = "%{y}: %{x}<extra></extra>") |>
      estilo_plotly(leyenda = FALSE) |>
      layout(xaxis = list(title = "Noticias"), yaxis = list(title = ""))
  })

  output$f_palabras <- renderPlotly({
    req(input$f_fuentes)
    d <- D$palabras_semana |> filter(fuente %in% input$f_fuentes) |>
      group_by(fuente, palabra) |> summarise(n = sum(n), .groups = "drop") |>
      group_by(fuente) |> slice_max(n, n = 6, with_ties = FALSE) |> ungroup()
    validate(need(nrow(d) > 0, "Sin datos"))
    plot_ly(d, x = ~n, y = ~reorder(palabra, n), color = ~fuente, colors = PALETA_CGR,
            type = "bar", orientation = "h",
            hovertemplate = "%{y}: %{x}<extra>%{fullData.name}</extra>") |>
      estilo_plotly() |>
      layout(barmode = "stack", xaxis = list(title = "Frecuencia"), yaxis = list(title = ""))
  })

  output$f_tfidf <- renderDT({
    req(input$f_fuentes)
    d <- D$tfidf_fuente |> filter(fuente %in% input$f_fuentes) |>
      group_by(fuente) |> slice_max(tf_idf, n = 8, with_ties = FALSE) |> ungroup() |>
      transmute(fuente = fuente, termino = palabra, frecuencia = n,
                tfidf = round(tf_idf, 4)) |>
      arrange(fuente, desc(tfidf))
    datatable(d, rownames = FALSE,
              colnames = c("Fuente", "Término", "Frecuencia", "TF-IDF"),
              options = list(pageLength = 10, dom = "tp",
              language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
  })

  # ===== NOTICIAS =====
  noticias_filtradas <- reactive({
    d <- D$datos |>
      filter(fuente %in% input$n_fuentes,
             fecha >= input$n_fechas[1], fecha <= input$n_fechas[2])
    if (nzchar(input$n_busqueda)) {
      d <- d |> filter(str_detect(str_to_lower(titulo), str_to_lower(input$n_busqueda)))
    }
    d |> arrange(desc(fecha))
  })

  output$n_titulo_tabla <- renderText({
    paste0(nrow(noticias_filtradas()), " noticias encontradas")
  })

  output$n_tabla <- renderDT({
    d <- noticias_filtradas() |>
      left_join(select(D$tono_articulo, id, clase), by = "id") |>
      mutate(clase = tidyr::replace_na(clase, "neutro")) |>
      transmute(
        fecha = format(fecha, "%Y-%m-%d"),
        fuente = fuente,
        titulo = ifelse(is.na(url) | url == "" | startsWith(url, "muestra2025"),
                        htmltools::htmlEscape(titulo),
                        paste0("<a href='", url, "' target='_blank'>",
                               htmltools::htmlEscape(titulo), "</a>")),
        tono = paste0("<span class='cgr-badge tono-", clase, "'>", clase, "</span>"),
        categorias = ifelse(is.na(categorias), "", categorias)
      )
    datatable(d, rownames = FALSE, escape = FALSE,
              colnames = c("Fecha", "Fuente", "Título", "Tono", "Categorías"),
              options = list(pageLength = 15, order = list(list(0, "desc")),
                language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
  })

  # ----- KWIC: palabra en contexto (insensible a tildes y mayúsculas) -----
  output$kwic_tabla <- renderDT({
    term <- str_trim(input$kwic_termino %||% "")
    validate(need(nchar(term) >= 2, "Escribe al menos 2 caracteres en la barra lateral."))

    term_norm <- stringi::stri_trans_general(tolower(term), "Latin-ASCII")
    term_esc  <- str_replace_all(term_norm,
                  "([\\.\\+\\*\\?\\(\\)\\[\\]\\{\\}\\^\\$\\|\\\\])", "\\\\\\1")
    re_match  <- regex(paste0("\\b", term_esc, "\\b"), ignore_case = FALSE)
    re_capt   <- regex(paste0("(.{0,80})\\b(", term_esc, ")\\b(.{0,80})"),
                       ignore_case = FALSE, dotall = TRUE)

    base <- noticias_filtradas() |>
      mutate(
        texto_orig = paste(titulo, replace_na(bajada, ""), replace_na(cuerpo, "")),
        texto_norm = stringi::stri_trans_general(tolower(texto_orig), "Latin-ASCII")
      ) |>
      filter(str_detect(texto_norm, re_match))

    validate(need(nrow(base) > 0, "Sin ocurrencias del término en las noticias filtradas."))

    base <- base |> head(80)
    # Latin-ASCII de español es char-a-char, así que las posiciones del match
    # en el texto normalizado se pueden usar sobre el texto original.
    pos <- str_locate(base$texto_norm, re_match)
    extra <- str_match(base$texto_norm, re_capt)
    izq_len <- nchar(extra[, 2]);  der_len <- nchar(extra[, 4])
    izq <- substr(base$texto_orig, pmax(1, pos[, 1] - izq_len), pos[, 1] - 1)
    mid <- substr(base$texto_orig, pos[, 1], pos[, 2])
    der <- substr(base$texto_orig, pos[, 2] + 1, pos[, 2] + der_len)

    contexto <- paste0(
      "…", htmltools::htmlEscape(izq),
      "<strong style='background:rgba(116,206,196,0.45);padding:0 3px;border-radius:3px;color:#1B1F49;'>",
      htmltools::htmlEscape(mid), "</strong>",
      htmltools::htmlEscape(der), "…"
    )

    d <- tibble(
      fecha   = format(base$fecha, "%Y-%m-%d"),
      fuente  = base$fuente,
      contexto = contexto
    )
    datatable(d, rownames = FALSE, escape = FALSE,
              colnames = c("Fecha", "Fuente", "Contexto"),
              options = list(pageLength = 10, dom = "tp",
                columnDefs = list(list(width = "65%", targets = 2)),
                language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
  })

}

shinyApp(ui, server)
