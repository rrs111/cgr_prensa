# CGR Prensa — Monitor de Medios

Monitoreo de la cobertura de la prensa chilena sobre la **Contraloría General de
la República de Chile (CGR)** y temas afines (fiscalización, probidad,
transparencia, auditoría pública, anticorrupción).

El proyecto scrapea 17 medios chilenos, filtra las noticias relacionadas con la
CGR, las procesa con análisis de texto y las muestra en una app Shiny con
estética corporativa. La arquitectura está basada en
[`prensa_chile`](https://github.com/bastianolea/prensa_chile) de Bastián Olea,
adaptada a esta temática.

---

## Flujo del proyecto

```
  instalar  ──►  scraping  ──►  procesar  ──►  app
```

```bash
# 1. Instalar dependencias (una vez)
Rscript instalar_paquetes.R

# 2. Scraping de los medios (descarga TODO, sin filtrar)
Rscript scraping/cgr_scraping.R

# 3. Procesamiento: filtra por relevancia CGR + análisis de texto
Rscript cgr_procesar.R

# 4. Levantar la app
Rscript -e 'shiny::runApp("app")'
```

Todo funciona igual desde RStudio (abrir `cgr_prensa.Rproj` y ejecutar los
scripts) o desde terminal con `Rscript`. No se usa `callr` ni `rstudioapi`.

> La app funciona aunque todavía no haya datos: si no encuentra datos reales,
> genera **datos sintéticos de demostración** automáticamente.

---

## Estructura

```
cgr_prensa/
├── cgr_prensa.Rproj
├── instalar_paquetes.R          # instala todos los paquetes
├── funciones.R                  # helpers compartidos (red, limpieza, fechas)
├── cgr_procesar.R               # ORQUESTADOR de procesamiento (p1→p3)
├── scraping/
│   ├── cgr_scraping.R           # ORQUESTADOR de scraping (todas las fuentes)
│   ├── cgr_scraping_funciones.R # motor de scraping (polite + rvest + chromote)
│   ├── cgr_obtener_emol.R       # un módulo por medio (17 en total)
│   └── ...
├── procesamiento/
│   ├── cgr_p1_cargar_datos.R    # cargar + limpiar + dedup + FILTRO CGR
│   ├── cgr_p2_tokenizar.R       # tokenizar + stopwords + stemming
│   ├── cgr_p3_contar_palabras.R # frecuencias semana/fuente + TF-IDF
│   └── cgr_importar_muestra.R   # importar muestra externa (ej. 10.000 de 2025)
├── app/
│   ├── app.R                    # app Shiny (4 pestañas, plotly, paleta CGR)
│   └── www/cgr_styles.css       # estética corporativa
├── datos/
│   ├── cgr_terminos.R           # términos de relevancia CGR + stopwords
│   ├── stopwords_es.txt
│   └── *.parquet / *.rds        # datos procesados (generados por el pipeline)
├── .github/workflows/           # automatización (scraping diario / semanal)
├── .gitignore
└── README.md
```

---

## Scraping

Cada módulo `cgr_obtener_<medio>.R` scrapea **un** medio. Todos comparten el
motor de `scraping/cgr_scraping_funciones.R`, que:

- usa **`{polite}`** (respeta `robots.txt`) con _fallback_ a **`{httr2}`**;
- extrae **título** (`og:title`), **bajada** (`meta description`), **cuerpo**
  (selector CSS verificado por medio), **fecha**, **URL** y **fuente**;
- maneja errores con `tryCatch()` sin detener la ejecución;
- pausa 1–3 s (aleatorio) entre solicitudes;
- guarda en `scraping/datos/{fuente}/*.rds`;
- tiene modo **reciente** y **histórico** (paginación), vía variables de entorno.

**Scraping por búsqueda (clave para el volumen):** para un monitor temático, lo
más eficiente es **buscar los términos CGR en el buscador de cada medio** (mucha
más cobertura que scrapear secciones generales y filtrar el ~25%). 11 medios
soportan búsqueda por URL (WordPress `?s=`, BioBío, La Tercera, Interferencia…);
cada uno se consulta con los términos de `terminos_busqueda`. El resto se cubre
por secciones. El filtro fino de relevancia se aplica igualmente en el
procesamiento, así que la búsqueda solo necesita surtir candidatos.

> Una sola búsqueda de "contraloria" devuelve ~60-100 enlaces en El Mostrador,
> Pauta o La Tercera. El scraping **no** filtra por keyword: el filtro CGR va en
> el procesamiento, para que los datos sirvan también a otros análisis.

### Variables de entorno del scraping

| Variable           | Efecto                                                          | Default   |
|--------------------|-----------------------------------------------------------------|-----------|
| `CGR_MODO`         | `completo` (búsqueda+secciones) · `buscar` · `reciente` · `hist`| completo  |
| `CGR_HIST_PAGINAS` | nº de páginas a recorrer en modo histórico                      | 5         |
| `CGR_MAX`          | tope de noticias por fuente                                     | sin tope  |
| `CGR_FUENTES`      | lista separada por comas, para correr solo algunas              | todas     |

```bash
# ejemplos
CGR_MODO=buscar Rscript scraping/cgr_scraping.R          # solo búsquedas CGR
CGR_MAX=30 Rscript scraping/cgr_scraping.R               # completo, tope 30/fuente
CGR_MODO=hist CGR_HIST_PAGINAS=10 Rscript scraping/cgr_scraping.R
CGR_FUENTES="ciper,elmostrador" Rscript scraping/cgr_scraping.R
```

### Estado de las fuentes

Los selectores CSS fueron verificados visitando cada sitio en vivo
(2026-05-28). Estado por medio:

| Medio            | Método   | Búsqueda CGR | Estado |
|------------------|----------|--------------|--------|
| El Mostrador     | rvest    | ✅ `?s=` (~63) | ✅ completo |
| Pauta            | rvest    | ✅ `?s=` (~65) | ✅ completo |
| El Dínamo        | rvest    | ✅ `?s=` (~47) | ✅ completo |
| El Desconcierto  | rvest    | ✅ `?s=` (~48) | ✅ completo |
| BioBioChile      | rvest    | ✅ `buscador` (~39) | ✅ completo |
| CNN Chile        | rvest    | ✅ `?s=` (~19) | ✅ completo |
| CIPER            | rvest    | ✅ `?s=` (~16) | ✅ completo |
| Interferencia    | rvest    | ✅ `search/node` (~10) | ✅ completo |
| Cooperativa      | rvest    | — (secciones)  | ✅ completo |
| Emol             | rvest    | — (secciones)  | ✅ completo |
| T13              | rvest    | — (secciones)  | ✅ completo |
| El Líbero        | rvest    | — (secciones)  | ✅ completo |
| La Segunda       | rvest    | — (3 secciones)| ⚠️ paywall: título + bajada + fecha |
| Diario Financiero| rvest    | — (secciones)  | ⚠️ paywall/anti-bot: título + bajada |
| La Tercera       | chromote | ✅ `?s=` (~100) | ✅ requiere Chrome (render JS, Arc) |
| The Clinic       | chromote | —              | ❌ bloqueado por Cloudflare (ni Chrome pasa) |
| Ex-Ante          | chromote | —              | ❌ bloqueado por Cloudflare (ni Chrome pasa) |

> **chromote** necesita Chrome/Chromium instalado (`brew install --cask google-chrome`;
> los workflows de GitHub Actions ya lo instalan). En entornos restringidos/CI se
> lanza con `--no-sandbox`. **La Tercera** funciona así. **The Clinic** y **Ex-Ante**
> están tras Cloudflare y ni con Chrome headless se pueden scrapear (devuelven la
> página de desafío); sus módulos quedan por completitud y normalmente dan 0.
> Si no hay Chrome, esos módulos no devuelven datos (se registra) y el resto continúa.

---

## Filtrado de relevancia CGR

En `datos/cgr_terminos.R` se definen los términos (institucionales, funciones de
la CGR y temática afín). El filtro normaliza el texto (minúsculas, sin tildes) y
conserva las noticias que mencionan alguno de los términos. También etiqueta
cada noticia con las **categorías** que menciona (`institucional`, `funciones`,
`tematica`).

---

## Procesamiento de texto

1. **P1 – Cargar datos:** unifica los `.rds`, limpia, deduplica por URL,
   interpreta fechas, **filtra por relevancia CGR** y **acumula** con el corpus
   previo (`datos/cgr_datos.parquet`).
2. **P2 – Tokenizar:** `{tidytext}` + stopwords (español + propias) + stemming
   con `{SnowballC}` (agrupa conjugaciones en una sola forma).
3. **P3 – Contar palabras:** frecuencia por semana y por fuente; TF-IDF por
   fuente.

Salidas (en `datos/`): `cgr_datos.parquet`, `cgr_palabras_semana.parquet`,
`cgr_noticias_semana.parquet`, `cgr_tfidf_fuente.parquet`, `cgr_metricas.rds`.

### Importar una muestra externa (bootstrap del corpus)

`prensa_chile` publica una [muestra de 10.000 noticias de 2025](https://github.com/bastianolea/prensa_chile/tree/main/datos).
Se puede filtrar por relevancia CGR e inyectar al corpus de un golpe:

```bash
# coloca el CSV en datos/ (o pasa la ruta) y luego:
Rscript procesamiento/cgr_importar_muestra.R datos/prensa_datos_muestra_2025.csv
Rscript cgr_procesar.R    # recalcula tokens/conteos
```

> En la práctica, ~1.600 de esas 10.000 noticias resultan relevantes a la CGR.
> El CSV grande no se versiona; solo entran al corpus las noticias filtradas.

---

## App Shiny

4 pestañas, gráficos interactivos con **`{plotly}`** y estética corporativa CGR
(paleta **Navy `#1B1F49` / Teal `#74CEC4` / Rosa `#F2567A` / Crema `#F4F2E5`**,
tipografías **DM Sans** + **DM Serif Display**):

1. **Resumen** — métricas, noticias por semana, distribución por fuente, top
   palabras, nube de palabras.
2. **Tendencias** — evolución temporal de hasta 5 términos, filtros por fuente y
   fecha, palabras emergentes (últimas 2 semanas vs. anteriores).
3. **Fuentes** — comparación entre medios: conteo, palabras frecuentes, TF-IDF.
4. **Noticias** — buscador con filtros y tabla con enlaces a las noticias.

```bash
Rscript -e 'shiny::runApp("app")'
```

> **Despliegue (shinyapps.io):** el bundle solo incluye la carpeta `app/`. Para
> desplegar con datos reales, copia los `.parquet`/`.rds` procesados a
> `app/datos/`. Si no hay datos, la app usa datos sintéticos de demostración.

---

## Automatización (GitHub Actions)

- **`scraping_diario.yaml`** — cada día a las 06:00 (hora Chile): scrapea modo
  reciente, procesa y hace commit de los datos procesados.
- **`procesar_semanal.yaml`** — domingos 08:00 (hora Chile): scraping histórico
  + procesamiento completo + commit.

Ambos instalan R + Chrome, cachean los paquetes y persisten el corpus acumulado
en `datos/` (los `.rds` crudos del scraping no se versionan).

---

## Paquetes

`tidyverse` · `glue` · `lubridate` · `rvest` · `polite` · `httr2` · `chromote` ·
`tidytext` · `stopwords` · `SnowballC` · `arrow` · `digest` · `fs` ·
`textclean` · `shiny` · `bslib` · `plotly` · `DT` · `shinyWidgets` ·
`shinycssloaders` · `future` · `furrr`.

> La nube de palabras se dibuja con **plotly** (no `wordcloud2`): wordcloud2
> inyecta dependencias JS antiguas que dejan en blanco los gráficos plotly de la
> misma página.

Requiere **R ≥ 4.1** (pipe nativo `|>`).

---

## Créditos

Arquitectura inspirada en [`prensa_chile`](https://github.com/bastianolea/prensa_chile)
de [Bastián Olea Herrera](https://bastian.olea.biz).
