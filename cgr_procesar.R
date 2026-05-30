# CGR PRENSA — ORQUESTADOR DE PROCESAMIENTO
# Ejecuta el pipeline completo de procesamiento de texto sobre los datos
# scrapeados. Compatible con RStudio y con `Rscript cgr_procesar.R`
# (NO usa callr ni rstudioapi).
#
#   1. Cargar + limpiar + deduplicar + FILTRAR por relevancia CGR
#   2. Tokenizar (stopwords + stemming)
#   3. Conteos por semana / fuente + TF-IDF
#   4. Tono / sentimiento de la cobertura (léxico ES)
#
# Uso:  Rscript cgr_procesar.R

suppressPackageStartupMessages(library(lubridate))
source("funciones.R")

inicio <- now()
notificacion("CGR Prensa", "Iniciando procesamiento…")

# Los objetos se comparten entre pasos en este entorno global para evitar
# relecturas innecesarias de disco (cada paso hace un fallback a read_parquet).
source("procesamiento/cgr_p1_cargar_datos.R")    # -> datos/cgr_datos.parquet
source("procesamiento/cgr_p2_tokenizar.R")       # -> datos/cgr_palabras.parquet
source("procesamiento/cgr_p3_contar_palabras.R") # -> conteos + tfidf
source("procesamiento/cgr_p4_sentimiento.R")     # -> tono por noticia/semana/fuente
source("procesamiento/cgr_p5_temas.R")           # -> temas latentes (stm)
source("procesamiento/cgr_p6_actores.R")         # -> entidades (NER con udpipe)

tiempo <- round(difftime(now(), inicio, units = "mins"), 2)
message(glue::glue("\n==== Procesamiento completo en {tiempo} min ===="))
notificacion("CGR Prensa", glue::glue("Procesamiento listo ({tiempo} min)"))
