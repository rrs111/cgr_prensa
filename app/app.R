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

  metricas <- list(
    total = nrow(datos),
    semana_actual = sum(datos$semana == max(semanas)),
    fuentes_activas = n_distinct(datos$fuente),
    ultima_fecha = max(datos$fecha),
    actualizado = now()
  )

  list(datos = datos, palabras_semana = palabras_semana,
       noticias_semana = noticias_semana, tfidf_fuente = tfidf_fuente,
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

  list(datos = datos, palabras_semana = palabras_semana,
       noticias_semana = noticias_semana, tfidf_fuente = tfidf_fuente,
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

  # ---- 2. TENDENCIAS ----
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
      transmute(
        fecha = format(fecha, "%Y-%m-%d"),
        fuente = fuente,
        titulo = paste0("<a href='", url, "' target='_blank'>", htmltools::htmlEscape(titulo), "</a>"),
        categorias = ifelse(is.na(categorias), "", categorias)
      )
    datatable(d, rownames = FALSE, escape = FALSE,
              colnames = c("Fecha", "Fuente", "Título", "Categorías"),
              options = list(pageLength = 15, order = list(list(0, "desc")),
                language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json")))
  })

}

shinyApp(ui, server)
