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
nube_plotly <- function(d, n_palabras = 70) {
  d <- d |> arrange(desc(freq)) |> head(n_palabras)
  k <- nrow(d)
  if (k == 0) return(plotly_empty())
  i <- seq_len(k)
  ang <- i * 2.399963          # ángulo áureo -> distribución pareja
  rad <- sqrt(i)
  fmin <- min(d$freq); fmax <- max(d$freq)
  size <- if (fmax > fmin) 13 + 34 * (sqrt(d$freq) - sqrt(fmin)) / (sqrt(fmax) - sqrt(fmin)) else rep(22, k)
  col <- rep(PALETA_CGR, length.out = k)
  plot_ly(
    x = rad * cos(ang), y = rad * sin(ang),
    type = "scatter", mode = "text",
    text = d$palabra, textfont = list(size = size, color = col, family = "DM Sans, sans-serif"),
    hovertext = paste0(d$palabra, ": ", d$freq), hoverinfo = "text"
  ) |>
    layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(visible = FALSE, range = c(-max(rad) - 1, max(rad) + 1)),
      yaxis = list(visible = FALSE, range = c(-max(rad) - 1, max(rad) + 1)),
      margin = list(t = 10, r = 10, b = 10, l = 10), showlegend = FALSE
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
cargar_archivo <- function(nombre) {
  cands <- c(file.path("datos", nombre),
             file.path("..", "datos", nombre),
             file.path("app", "datos", nombre))
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0) return(NULL)
  if (grepl("\\.parquet$", nombre)) {
    tryCatch(arrow::read_parquet(hit[1]), error = function(e) NULL)
  } else {
    tryCatch(readRDS(hit[1]), error = function(e) NULL)
  }
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

  tfidf_fuente <- palabras_semana |>
    count(fuente, palabra, wt = n, name = "n") |>
    tidytext::bind_tf_idf(palabra, fuente, n)

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
    "corrupcion · municipio · fondos",
    "auditoria · contrato · licitacion",
    "dictamen · decreto · toma de razon",
    "sumario · funcionarios · responsabilidad",
    "transparencia · probidad · declaracion",
    "alcalde · gobierno regional · gore",
    "presupuesto · gasto · ministerio"
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

  list(datos = datos, palabras_semana = palabras_semana,
       noticias_semana = noticias_semana, tfidf_fuente = tfidf_fuente,
       tono_semana = tono_semana, tono_fuente = tono_fuente, tono_articulo = tono_articulo,
       temas_terminos = temas_terminos, temas_doc = temas_doc, temas_semana = temas_semana,
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

  list(datos = datos, palabras_semana = palabras_semana,
       noticias_semana = noticias_semana, tfidf_fuente = tfidf_fuente,
       tono_semana = tono_semana, tono_fuente = tono_fuente, tono_articulo = tono_articulo,
       temas_terminos = temas_terminos, temas_doc = temas_doc, temas_semana = temas_semana,
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
          "CGR detecta corrupción tiende a tono negativo por el hecho que reporta.")
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

  # ---- 4. TENDENCIAS ----
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
           withSpinner(plotlyOutput("g_emergentes", height = 320)))
    )
  ),

  # ---- 3. FUENTES ----
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

  # ---- 4. NOTICIAS ----
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
                    value = rango_fechas, timeFormat = "%d %b %Y")
      ),
      card(card_header(textOutput("n_titulo_tabla")),
           withSpinner(DTOutput("n_tabla")))
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

}

shinyApp(ui, server)
