# Instalación de todos los paquetes necesarios para CGR Prensa
# Ejecutar una vez: Rscript instalar_paquetes.R  (o source() desde RStudio)

opciones_repo <- "https://cloud.r-project.org"

paquetes <- c(
  # core
  "tidyverse", "glue", "lubridate",
  # scraping
  "rvest", "polite", "httr2", "chromote",
  # texto
  "tidytext", "stopwords", "SnowballC", "textclean",
  # datos
  "arrow", "digest", "fs",
  # app (la nube de palabras se hace con plotly, no con wordcloud2, para evitar
  # un conflicto de dependencias JS que deja en blanco los gráficos plotly)
  "shiny", "plotly", "DT", "bslib",
  "shinyWidgets", "shinycssloaders", "htmltools",
  # paralelismo (opcional)
  "future", "furrr"
)

instalados <- rownames(installed.packages())
faltantes <- setdiff(paquetes, instalados)

if (length(faltantes) == 0) {
  message("Todos los paquetes ya están instalados.")
} else {
  message("Instalando: ", paste(faltantes, collapse = ", "))
  options(Ncpus = max(1, parallel::detectCores() - 1))
  install.packages(faltantes, repos = opciones_repo)
}

# chromote requiere un navegador Chrome/Chromium instalado en el sistema.
# Si vas a scrapear sitios con JavaScript (La Tercera, Ex-Ante, The Clinic, etc.),
# instala Chrome:  macOS -> brew install --cask google-chrome
# y verifica con:  chromote::find_chrome()
if ("chromote" %in% rownames(installed.packages())) {
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (is.null(chrome)) {
    message("\nAVISO: chromote no encontró Chrome. Los scrapers con JS no funcionarán ",
            "hasta instalar Chrome (brew install --cask google-chrome).")
  } else {
    message("\nChrome detectado para chromote: ", chrome)
  }
}
