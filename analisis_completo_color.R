# Proyecto Integrador P1 - Probabilidad y Estadística (UDLA, NRC 2769)
# Script reproducible. Coloque este archivo y el Excel en la misma carpeta.
# Si hacen falta paquetes: install.packages(c("readxl", "dplyr", "tidyr", "ggplot2"))

paquetes <- c("readxl", "dplyr", "tidyr", "ggplot2")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) {
  stop(paste0("Faltan paquetes. Ejecute: install.packages(c(",
              paste(sprintf('"%s"', faltan), collapse = ", "), "))"))
}

# 1. Localizar e importar el archivo como texto para auditar marcadores no numéricos.
archivo <- "Superficie y Producción.xlsx"
if (!file.exists(archivo)) {
  candidatos <- list.files(pattern = "Superficie.*Producci.*\\.xlsx$", ignore.case = TRUE,
                          full.names = TRUE)
  if (length(candidatos) > 0) archivo <- candidatos[1]
}
if (!file.exists(archivo)) {
  stop("No encuentro 'Superficie y Producción.xlsx'. Guarde el Excel junto a este script y establezca esa carpeta como directorio de trabajo.")
}
hojas <- readxl::excel_sheets(archivo)
hoja <- if ("Datos_Depurados" %in% hojas) "Datos_Depurados" else hojas[1]
datos_brutos <- readxl::read_excel(archivo, sheet = hoja,
                                   col_types = rep("text", 7),
                                   .name_repair = "minimal")

columnas_esperadas <- c("Año", "Cultivo", "Provincia", "Tipo",
                        "Superficie Plantada (ha)",
                        "Superficie Cosechada (ha)",
                        "Producción (toneladas)")
if (!all(columnas_esperadas %in% names(datos_brutos))) {
  stop(paste0("Los encabezados no coinciden con los esperados. Encabezados encontrados: ",
              paste(names(datos_brutos), collapse = " | ")))
}

# Conserva valores ausentes como NA: nunca transforma *, ., .. o , en cero.
normaliza_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  z <- trimws(as.character(x))
  tokens_ausentes <- c("", "*", ".", "..", ",", "NA", "N/A", "n/a", "-")
  z[z %in% tokens_ausentes] <- NA_character_
  # Quita comas de millares tipo 1,234.56; admite coma decimal si no hay punto.
  z <- gsub("(?<=\\d),(?=\\d{3}(\\D|$))", "", z, perl = TRUE)
  idx_coma_decimal <- !is.na(z) & grepl(",", z, fixed = TRUE) & !grepl(".", z, fixed = TRUE)
  z[idx_coma_decimal] <- sub(",", ".", z[idx_coma_decimal], fixed = TRUE)
  suppressWarnings(as.numeric(z))
}

texto_limpio <- function(x) {
  z <- trimws(as.character(x))
  z[z == ""] <- NA_character_
  z
}

datos <- data.frame(
  anio = normaliza_num(datos_brutos[["Año"]]),
  cultivo = texto_limpio(datos_brutos[["Cultivo"]]),
  provincia = texto_limpio(datos_brutos[["Provincia"]]),
  tipo = texto_limpio(datos_brutos[["Tipo"]]),
  superficie_plantada = normaliza_num(datos_brutos[["Superficie Plantada (ha)"]]),
  superficie_cosechada = normaliza_num(datos_brutos[["Superficie Cosechada (ha)"]]),
  produccion = normaliza_num(datos_brutos[["Producción (toneladas)"]]),
  stringsAsFactors = FALSE
)
variables_num <- c("superficie_plantada", "superficie_cosechada", "produccion")
labels_num <- c(superficie_plantada = "Superficie plantada (ha)",
                superficie_cosechada = "Superficie cosechada (ha)",
                produccion = "Producción (toneladas)")

# 2. Auditoría de faltantes y marcadores en las columnas cuantitativas.
auditar_token <- function(x, token) sum(trimws(as.character(x)) == token, na.rm = TRUE)
auditoria_tokens <- do.call(rbind, lapply(columnas_esperadas[5:7], function(v) {
  x <- datos_brutos[[v]]
  data.frame(
    variable = v,
    vacios_o_NA = sum(is.na(x) | trimws(as.character(x)) == ""),
    asterisco = auditar_token(x, "*"),
    punto = auditar_token(x, "."),
    doble_punto = auditar_token(x, ".."),
    coma_aislada = auditar_token(x, ","),
    no_convertibles = sum(is.na(normaliza_num(x)) & !is.na(x) &
      !(trimws(as.character(x)) %in% c("", "*", ".", "..", ",", "NA", "N/A", "n/a", "-"))),
    stringsAsFactors = FALSE
  )
}))

n_tipo_original <- length(unique(as.character(datos_brutos[["Tipo"]][!is.na(datos_brutos[["Tipo"]])])))
n_tipo_limpio <- length(unique(datos$tipo[!is.na(datos$tipo)]))
espacios_finales <- sum(vapply(c("Cultivo", "Provincia", "Tipo"), function(v) {
  x <- as.character(datos_brutos[[v]])
  sum(!is.na(x) & x != trimws(x))
}, numeric(1)))

claves <- c("anio", "cultivo", "provincia", "tipo")
claves_repetidas <- datos |>
  dplyr::count(dplyr::across(dplyr::all_of(claves)), name = "n") |>
  dplyr::filter(n > 1)
duplicados_exactos_originales <- sum(duplicated(datos_brutos))
duplicados_tras_normalizar <- sum(duplicated(datos))
negativos <- sum(vapply(datos[variables_num], function(x) sum(x < 0, na.rm = TRUE), numeric(1)))

conteo_vacios <- sum(vapply(datos, function(x) sum(is.na(x) | (is.character(x) & trimws(x) == "")), numeric(1)))
no_convertibles_total <- sum(auditoria_tokens$no_convertibles)
auditoria_calidad <- data.frame(
  Indicador = c("Archivo importado", "Hoja utilizada", "Filas leídas (sin encabezado)",
    "Columnas leídas", "Año inicial", "Año final", "Cultivos distintos tras normalizar etiquetas",
    "Provincias/agrupaciones distintas", "Tipos distintos antes de recortar espacios",
    "Tipos distintos después de recortar espacios", "Etiquetas con espacios iniciales/finales corregidas",
    "Campos vacíos/NA detectados", "Marcadores *, ., .. o coma aislada en variables cuantitativas",
    "Valores cuantitativos no convertibles", "Valores cuantitativos negativos",
    "Duplicados exactos en archivo original", "Duplicados después de normalizar",
    "Filas extra con clave año-cultivo-provincia-tipo repetida"),
  Resultado = c(basename(archivo), hoja, nrow(datos), ncol(datos),
    min(datos$anio, na.rm = TRUE), max(datos$anio, na.rm = TRUE),
    length(unique(stats::na.omit(datos$cultivo))), length(unique(stats::na.omit(datos$provincia))),
    n_tipo_original, n_tipo_limpio, espacios_finales, conteo_vacios,
    sum(rowSums(auditoria_tokens[, c("asterisco", "punto", "doble_punto", "coma_aislada")]) ),
    no_convertibles_total, negativos, duplicados_exactos_originales,
    duplicados_tras_normalizar, sum(claves_repetidas$n - 1)),
  stringsAsFactors = FALSE
)

# 3. Frecuencias. El conteo representa registros de la base, no hectáreas ni fincas.
freq_cultivo <- datos |>
  dplyr::filter(!is.na(cultivo)) |>
  dplyr::count(cultivo, sort = TRUE, name = "registros") |>
  dplyr::mutate(porcentaje = 100 * registros / sum(registros))
freq_tipo <- datos |>
  dplyr::filter(!is.na(tipo)) |>
  dplyr::count(tipo, sort = TRUE, name = "registros") |>
  dplyr::mutate(porcentaje = 100 * registros / sum(registros))
freq_anio <- datos |>
  dplyr::filter(!is.na(anio)) |>
  dplyr::count(anio, name = "registros") |>
  dplyr::arrange(anio)
freq_provincia <- datos |>
  dplyr::filter(!is.na(provincia)) |>
  dplyr::count(provincia, sort = TRUE, name = "registros") |>
  dplyr::mutate(porcentaje = 100 * registros / sum(registros))
top_cultivos <- utils::head(freq_cultivo$cultivo, 8)
top_cultivos_12 <- utils::head(freq_cultivo, 12)
top_provincias_10 <- utils::head(freq_provincia, 10)

# 4. Estadística descriptiva de las tres variables cuantitativas.
moda_num <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_character_)
  tab <- table(x)
  max_f <- max(tab)
  if (max_f <= 1) return("Sin moda repetida")
  mods <- names(tab)[as.integer(tab) == max_f]
  paste(utils::head(signif(as.numeric(mods), 6), 3), collapse = "; ")
}
resumir_variable <- function(x, variable, nombre) {
  n_total <- length(x)
  z <- x[is.finite(x)]
  if (length(z) == 0) return(data.frame())
  q <- as.numeric(stats::quantile(z, probs = c(.25, .5, .75), na.rm = TRUE, names = FALSE, type = 7))
  media <- mean(z)
  m2 <- mean((z - media)^2)
  q1 <- q[1]; mediana <- q[2]; q3 <- q[3]
  iqr <- q3 - q1
  limite_inf <- q1 - 1.5 * iqr
  limite_sup <- q3 + 1.5 * iqr
  asimetria <- if (m2 > 0) mean((z - media)^3) / (m2^(3/2)) else NA_real_
  curtosis_exceso <- if (m2 > 0) mean((z - media)^4) / (m2^2) - 3 else NA_real_
  data.frame(
    variable = nombre, n_valido = length(z), faltantes = n_total - length(z),
    media = media, mediana = mediana, moda = moda_num(z), minimo = min(z),
    Q1 = q1, Q3 = q3, maximo = max(z), rango = max(z) - min(z), IQR = iqr,
    varianza_muestral = stats::var(z), desviacion_estandar = stats::sd(z),
    coeficiente_variacion_pct = if (media != 0) stats::sd(z) / abs(media) * 100 else NA_real_,
    asimetria = asimetria, curtosis_exceso = curtosis_exceso,
    atipicos_Tukey = sum(z < limite_inf | z > limite_sup),
    ceros = sum(z == 0), stringsAsFactors = FALSE
  )
}
resumen_cuantitativas <- do.call(rbind, lapply(variables_num, function(v) {
  resumir_variable(datos[[v]], v, labels_num[[v]])
}))

# 5. Tabla de contingencia Cultivo x Tipo (ocho cultivos con más registros).
# Se incluyen frecuencias absolutas, porcentajes por fila, por columna y del total general.
datos_cont <- datos |>
  dplyr::filter(cultivo %in% top_cultivos, !is.na(tipo))
tabla_cont <- table(factor(datos_cont$cultivo, levels = top_cultivos),
                    factor(datos_cont$tipo, levels = unique(datos$tipo[!is.na(datos$tipo)])))
matriz_conteos <- as.data.frame.matrix(tabla_cont)
contingencia_conteos <- data.frame(
  Cultivo = rownames(matriz_conteos), matriz_conteos,
  Total = as.numeric(rowSums(tabla_cont)),
  check.names = FALSE, row.names = NULL
)
# Fila de totales marginales, equivalente al n de una tabla de contingencia.
fila_total <- data.frame(
  Cultivo = "TOTAL GENERAL",
  as.data.frame.matrix(matrix(as.numeric(colSums(tabla_cont)), nrow = 1,
                              dimnames = list(NULL, colnames(tabla_cont)))),
  Total = as.numeric(sum(tabla_cont)), check.names = FALSE
)
names(fila_total) <- names(contingencia_conteos)
contingencia_conteos <- rbind(contingencia_conteos, fila_total)

# Porcentaje por fila: cada cultivo suma aproximadamente 100%.
contingencia_fila_pct <- 100 * prop.table(tabla_cont, margin = 1)
contingencia_porcentajes <- data.frame(
  Cultivo = rownames(as.data.frame.matrix(contingencia_fila_pct)),
  round(as.data.frame.matrix(contingencia_fila_pct), 2),
  `Total fila (%)` = round(rowSums(contingencia_fila_pct), 2),
  check.names = FALSE, row.names = NULL
)

# Porcentaje por columna: cada tipo suma aproximadamente 100% dentro de los ocho cultivos.
contingencia_columna_pct <- 100 * prop.table(tabla_cont, margin = 2)
contingencia_porcentajes_columna <- data.frame(
  Cultivo = rownames(as.data.frame.matrix(contingencia_columna_pct)),
  round(as.data.frame.matrix(contingencia_columna_pct), 2),
  check.names = FALSE, row.names = NULL
)

# Porcentaje de la base completa: permite ver qué parte de los 14.405 registros
# corresponde a cada celda de esta tabla parcial (solo ocho cultivos).
contingencia_total_pct <- 100 * tabla_cont / max(1, nrow(datos))
contingencia_porcentajes_total <- data.frame(
  Cultivo = rownames(as.data.frame.matrix(contingencia_total_pct)),
  round(as.data.frame.matrix(contingencia_total_pct), 3),
  `Total de la base (%)` = round(rowSums(contingencia_total_pct), 3),
  check.names = FALSE, row.names = NULL
)
fila_pct_total <- data.frame(
  Cultivo = "TOTAL 8 CULTIVOS (%)",
  as.data.frame.matrix(matrix(round(colSums(contingencia_total_pct), 3), nrow = 1,
                              dimnames = list(NULL, colnames(tabla_cont)))),
  `Total de la base (%)` = round(sum(contingencia_total_pct), 3),
  check.names = FALSE
)
names(fila_pct_total) <- names(contingencia_porcentajes_total)
contingencia_porcentajes_total <- rbind(contingencia_porcentajes_total, fila_pct_total)

contingencia_long <- datos_cont |>
  dplyr::count(cultivo, tipo, name = "registros") |>
  dplyr::group_by(cultivo) |>
  dplyr::mutate(porcentaje_fila = 100 * registros / sum(registros)) |>
  dplyr::ungroup()

# 5B. Tabla complementaria Provincia x Cultivo: porcentaje condicionado a la provincia.
# Mantiene todas las categorias en el denominador; en el informe se muestra una vista
# reducida (7 provincias x 6 cultivos) para que sea legible en formato IEEE.
provincias_orden <- sort(unique(datos$provincia[!is.na(datos$provincia)]))
cultivos_orden <- sort(unique(datos$cultivo[!is.na(datos$cultivo)]))
tabla_provincia_cultivo <- table(
  factor(datos$provincia, levels = provincias_orden),
  factor(datos$cultivo, levels = cultivos_orden)
)
pct_provincia_cultivo <- 100 * prop.table(tabla_provincia_cultivo, margin = 1)
pct_provincia_cultivo_completa <- data.frame(
  Provincia = rownames(as.data.frame.matrix(pct_provincia_cultivo, make.names = FALSE)),
  round(as.data.frame.matrix(pct_provincia_cultivo, make.names = FALSE), 2),
  `Total fila (%)` = round(rowSums(pct_provincia_cultivo), 2),
  check.names = FALSE, row.names = NULL
)
# Orden fijo del extracto que se muestra en el informe para que el resultado no dependa
# del idioma/configuración regional usada por sort() en cada instalación de R.
provincias_preferidas <- c("Azuay", "Bolívar", "Cañar", "Carchi",
                           "Centro-Suroriente", "Chimborazo", "Cotopaxi")
provincias_muestra <- provincias_preferidas[provincias_preferidas %in% provincias_orden]
if (length(provincias_muestra) < 7) {
  provincias_muestra <- c(provincias_muestra,
    head(setdiff(provincias_orden, provincias_muestra), 7 - length(provincias_muestra)))
}
cultivos_preferidos <- c(
  "Aguacate (Fruta Fresca)", "Arroz (En Cáscara)",
  "Arveja Seca (Grano Seco)", "Arveja Tierna (En Vaina)",
  "Banano (Fruta Fresca)", "Banano De Exportación En Monocultivo (Fruta Fresca)"
)
cultivos_muestra <- cultivos_preferidos[cultivos_preferidos %in% cultivos_orden]
if (length(cultivos_muestra) < 6) {
  cultivos_muestra <- c(cultivos_muestra,
    head(setdiff(cultivos_orden, cultivos_muestra), 6 - length(cultivos_muestra)))
}

# Etiquetas cortas para que las tablas sean legibles en el PDF, sin cambiar las categorías originales.
etiquetas_cultivos <- c(
  "Aguacate (Fruta Fresca)" = "Aguacate",
  "Arroz (En Cáscara)" = "Arroz",
  "Arveja Seca (Grano Seco)" = "Arveja seca",
  "Arveja Tierna (En Vaina)" = "Arveja tierna",
  "Banano (Fruta Fresca)" = "Banano",
  "Banano De Exportación En Monocultivo (Fruta Fresca)" = "Banano exportación"
)
etiquetas_cultivos_muestra <- unname(etiquetas_cultivos[cultivos_muestra])
for (i in seq_along(etiquetas_cultivos_muestra)) {
  if (is.na(etiquetas_cultivos_muestra[i]) || !nzchar(etiquetas_cultivos_muestra[i])) {
    etiquetas_cultivos_muestra[i] <- sub("\\s*\\(.*$", "", cultivos_muestra[i])
  }
}

# Tabla de frecuencias absolutas completa: conserva todas las provincias y cultivos.
matriz_provincia_cultivo_conteos <- as.data.frame.matrix(tabla_provincia_cultivo, make.names = FALSE)
tabla_provincia_cultivo_conteos_completa <- data.frame(
  Provincia = rownames(matriz_provincia_cultivo_conteos),
  matriz_provincia_cultivo_conteos,
  `Total registros provincia` = as.numeric(rowSums(tabla_provincia_cultivo)),
  check.names = FALSE, row.names = NULL
)
fila_total_provincia_conteos <- data.frame(
  Provincia = "TOTAL GENERAL",
  as.data.frame.matrix(matrix(
    as.numeric(colSums(tabla_provincia_cultivo)), nrow = 1,
    dimnames = list(NULL, cultivos_orden)
  ), make.names = FALSE),
  `Total registros provincia` = nrow(datos),
  check.names = FALSE
)
names(fila_total_provincia_conteos) <- names(tabla_provincia_cultivo_conteos_completa)
tabla_provincia_cultivo_conteos_completa <- rbind(
  tabla_provincia_cultivo_conteos_completa, fila_total_provincia_conteos
)

# Vista legible para el informe: siete territorios, seis cultivos y total de todos los cultivos.
matriz_conteos_muestra <- as.data.frame.matrix(
  tabla_provincia_cultivo[provincias_muestra, cultivos_muestra, drop = FALSE],
  make.names = FALSE
)
names(matriz_conteos_muestra) <- etiquetas_cultivos_muestra
tabla_provincia_cultivo_conteos_muestra <- data.frame(
  Provincia = provincias_muestra,
  matriz_conteos_muestra,
  `Total provincia` = as.numeric(rowSums(
    tabla_provincia_cultivo[provincias_muestra, , drop = FALSE]
  )),
  check.names = FALSE, row.names = NULL
)
fila_total_muestra_conteos <- data.frame(
  Provincia = "TOTAL GENERAL",
  as.data.frame.matrix(matrix(
    as.numeric(colSums(tabla_provincia_cultivo[, cultivos_muestra, drop = FALSE])),
    nrow = 1, dimnames = list(NULL, etiquetas_cultivos_muestra)
  ), make.names = FALSE),
  `Total provincia` = nrow(datos),
  check.names = FALSE
)
names(fila_total_muestra_conteos) <- names(tabla_provincia_cultivo_conteos_muestra)
tabla_provincia_cultivo_conteos_muestra <- rbind(
  tabla_provincia_cultivo_conteos_muestra, fila_total_muestra_conteos
)

# Tabla complementaria de porcentajes por fila: denominador = todos los cultivos de cada provincia.
matriz_pct_muestra <- as.data.frame.matrix(
  pct_provincia_cultivo[provincias_muestra, cultivos_muestra, drop = FALSE],
  make.names = FALSE
)
names(matriz_pct_muestra) <- etiquetas_cultivos_muestra
tabla_provincia_cultivo_muestra <- data.frame(
  Provincia = provincias_muestra,
  round(matriz_pct_muestra, 2),
  `Total (%)` = round(rowSums(
    pct_provincia_cultivo[provincias_muestra, , drop = FALSE]
  ), 2),
  check.names = FALSE, row.names = NULL
)

# 6. Correlación de Pearson: total y cultivo más frecuente. Para esta relación física,
# se excluyen únicamente pares negativos/no finitos; el conteo de negativos queda en la auditoría.
datos_cor_total <- datos |>
  dplyr::filter(is.finite(superficie_cosechada), is.finite(produccion),
                superficie_cosechada >= 0, produccion >= 0)
cultivo_correlacion <- top_cultivos[1]
datos_cor_cultivo <- datos |>
  dplyr::filter(cultivo == cultivo_correlacion,
                is.finite(superficie_cosechada), is.finite(produccion),
                superficie_cosechada >= 0, produccion >= 0)
if (nrow(datos_cor_total) < 3 || stats::sd(datos_cor_total$superficie_cosechada) == 0 ||
    stats::sd(datos_cor_total$produccion) == 0) stop("No es posible calcular correlación global: revisar variables.")
if (nrow(datos_cor_cultivo) < 3 || stats::sd(datos_cor_cultivo$superficie_cosechada) == 0 ||
    stats::sd(datos_cor_cultivo$produccion) == 0) stop("No es posible calcular correlación para el cultivo seleccionado.")
test_cor_total <- stats::cor.test(datos_cor_total$superficie_cosechada,
                                  datos_cor_total$produccion, method = "pearson")
test_cor_cultivo <- stats::cor.test(datos_cor_cultivo$superficie_cosechada,
                                    datos_cor_cultivo$produccion, method = "pearson")
resumen_correlaciones <- data.frame(
  Universo = c("Todos los cultivos", cultivo_correlacion),
  n_pares = c(nrow(datos_cor_total), nrow(datos_cor_cultivo)),
  r_Pearson = c(unname(test_cor_total$estimate), unname(test_cor_cultivo$estimate)),
  R2_descriptivo = c(unname(test_cor_total$estimate)^2, unname(test_cor_cultivo$estimate)^2),
  p_valor = c(test_cor_total$p.value, test_cor_cultivo$p.value),
  IC95_inferior = c(test_cor_total$conf.int[1], test_cor_cultivo$conf.int[1]),
  IC95_superior = c(test_cor_total$conf.int[2], test_cor_cultivo$conf.int[2]),
  stringsAsFactors = FALSE
)

# 7. Gráficos reproducibles, con paleta coherente y guardados en PNG a 300 dpi.
dir.create("salidas_proyecto", showWarnings = FALSE, recursive = TRUE)

paleta_graf <- c("#2563EB", "#0F766E", "#F59E0B", "#8B5CF6", "#EC4899",
                 "#0891B2", "#65A30D", "#EA580C", "#4F46E5", "#14B8A6",
                 "#9333EA", "#DC2626")
tema_color <- ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", colour = "#17324D", size = 12),
    plot.subtitle = ggplot2::element_text(colour = "#475569", size = 9),
    axis.title = ggplot2::element_text(face = "bold", colour = "#334155"),
    axis.text = ggplot2::element_text(colour = "#334155"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "#E2E8F0", linewidth = 0.35),
    legend.title = ggplot2::element_text(face = "bold", colour = "#334155"),
    legend.text = ggplot2::element_text(colour = "#334155")
  )

niveles_cultivos <- levels(reorder(top_cultivos_12$cultivo, top_cultivos_12$registros))
colores_cultivos <- stats::setNames(rep(paleta_graf, length.out = length(niveles_cultivos)), niveles_cultivos)
p_cultivos <- ggplot2::ggplot(top_cultivos_12,
  ggplot2::aes(x = reorder(cultivo, registros), y = registros,
               fill = reorder(cultivo, registros))) +
  ggplot2::geom_col(width = 0.78, colour = "white", linewidth = 0.25) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(values = colores_cultivos, guide = "none") +
  ggplot2::labs(title = "Cultivos con más registros", subtitle = "Frecuencia de filas en la base",
                x = "Cultivo", y = "Número de registros") + tema_color
 ggplot2::ggsave("salidas_proyecto/01_frecuencia_cultivos.png", p_cultivos, width = 7, height = 5, dpi = 300)

colores_tipo <- stats::setNames(rep(paleta_graf, length.out = nrow(freq_tipo)), freq_tipo$tipo)
p_tipo <- ggplot2::ggplot(freq_tipo,
  ggplot2::aes(x = reorder(tipo, registros), y = registros, fill = tipo)) +
  ggplot2::geom_col(width = 0.68, colour = "white", linewidth = 0.3) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(values = colores_tipo, guide = "none") +
  ggplot2::labs(title = "Frecuencia por modalidad de cultivo", x = "Tipo", y = "Número de registros") + tema_color
 ggplot2::ggsave("salidas_proyecto/02_frecuencia_tipo.png", p_tipo, width = 6, height = 3.5, dpi = 300)

p_anio <- ggplot2::ggplot(freq_anio, ggplot2::aes(x = anio, y = registros)) +
  ggplot2::geom_area(fill = "#BFDBFE", alpha = 0.8) +
  ggplot2::geom_line(colour = "#1D4ED8", linewidth = 0.9) +
  ggplot2::geom_point(colour = "#F97316", fill = "#FDBA74", shape = 21, size = 2.4, stroke = 0.6) +
  ggplot2::scale_x_continuous(breaks = seq(min(freq_anio$anio), max(freq_anio$anio), by = 2)) +
  ggplot2::labs(title = "Registros por año", x = "Año", y = "Número de registros") + tema_color
 ggplot2::ggsave("salidas_proyecto/03_registros_por_anio.png", p_anio, width = 7, height = 3.5, dpi = 300)

p_provincias <- ggplot2::ggplot(top_provincias_10,
  ggplot2::aes(x = reorder(provincia, registros), y = registros, fill = registros)) +
  ggplot2::geom_col(width = 0.78, colour = "white", linewidth = 0.25) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_gradient(low = "#FDE68A", high = "#C2410C", guide = "none") +
  ggplot2::labs(title = "Territorios con más registros", x = "Provincia o agrupación", y = "Número de registros") + tema_color
 ggplot2::ggsave("salidas_proyecto/04_frecuencia_provincias.png", p_provincias, width = 7, height = 4.5, dpi = 300)

crear_histograma <- function(variable, nombre_archivo) {
  x <- datos[[variable]]
  x <- x[is.finite(x) & x >= 0]
  tmp <- data.frame(valor_log10_mas_uno = log10(x + 1))
  tmp <- tmp[is.finite(tmp$valor_log10_mas_uno), , drop = FALSE]
  p <- ggplot2::ggplot(tmp, ggplot2::aes(x = valor_log10_mas_uno)) +
    ggplot2::geom_histogram(ggplot2::aes(fill = ggplot2::after_stat(count)), bins = 40,
                            colour = "white", linewidth = 0.25) +
    ggplot2::scale_fill_gradient(low = "#BFDBFE", high = "#1D4ED8", guide = "none") +
    ggplot2::labs(title = paste("Distribución de", labels_num[[variable]]),
      subtitle = "Escala transformada solo para visualizar la asimetría",
      x = "log10(valor + 1)", y = "Frecuencia") + tema_color
  ggplot2::ggsave(file.path("salidas_proyecto", nombre_archivo), p, width = 6.5, height = 3.8, dpi = 300)
  p
}
crear_boxplot <- function(variable, nombre_archivo, color_relleno) {
  x <- datos[[variable]]
  x <- x[is.finite(x) & x >= 0]
  tmp <- data.frame(valor_log10_mas_uno = log10(x + 1))
  tmp <- tmp[is.finite(tmp$valor_log10_mas_uno), , drop = FALSE]
  p <- ggplot2::ggplot(tmp, ggplot2::aes(x = "", y = valor_log10_mas_uno)) +
    ggplot2::geom_boxplot(fill = color_relleno, colour = "#1E3A8A", width = 0.42,
                          outlier.colour = "#F97316", outlier.alpha = 0.35, outlier.size = 1.2) +
    ggplot2::labs(title = paste("Diagrama de caja de", labels_num[[variable]]),
      x = NULL, y = "log10(valor + 1) [solo visualización]") + tema_color
  ggplot2::ggsave(file.path("salidas_proyecto", nombre_archivo), p, width = 5.8, height = 3.8, dpi = 300)
  p
}
p_hist_plantada <- crear_histograma("superficie_plantada", "05_hist_superficie_plantada.png")
p_hist_cosechada <- crear_histograma("superficie_cosechada", "06_hist_superficie_cosechada.png")
p_hist_produccion <- crear_histograma("produccion", "07_hist_produccion.png")
p_box_plantada <- crear_boxplot("superficie_plantada", "08_box_superficie_plantada.png", "#93C5FD")
p_box_cosechada <- crear_boxplot("superficie_cosechada", "09_box_superficie_cosechada.png", "#5EEAD4")
p_box_produccion <- crear_boxplot("produccion", "10_box_produccion.png", "#C4B5FD")

p_contingencia <- ggplot2::ggplot(contingencia_long,
  ggplot2::aes(x = reorder(cultivo, registros, FUN = sum), y = porcentaje_fila / 100, fill = tipo)) +
  ggplot2::geom_col(position = "fill", colour = "white", linewidth = 0.25, width = 0.72) +
  ggplot2::coord_flip() +
  # Acortar etiquetas para que no se recorten dentro de una columna IEEE.
  ggplot2::scale_x_discrete(labels = function(x) sub(" \\(.*$", "", x)) +
  ggplot2::scale_fill_manual(values = colores_tipo, breaks = names(colores_tipo)) +
  ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"),
                              breaks = seq(0, 1, by = 0.25), expand = c(0, 0)) +
  ggplot2::labs(title = "Composición de Tipo por cultivo",
                subtitle = "Ocho cultivos con más registros",
                x = NULL, y = "Porcentaje", fill = NULL) + tema_color +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", colour = "#17324D", size = 10),
    plot.subtitle = ggplot2::element_text(colour = "#475569", size = 7.5),
    axis.text.x = ggplot2::element_text(size = 7),
    axis.text.y = ggplot2::element_text(size = 7),
    axis.title.x = ggplot2::element_text(size = 8),
    axis.title.y = ggplot2::element_text(size = 8),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 7),
    legend.key.size = grid::unit(0.22, "cm"),
    legend.margin = ggplot2::margin(t = 0, b = 0),
    plot.margin = ggplot2::margin(4, 4, 4, 4)
  )
ggplot2::ggsave("salidas_proyecto/11_contingencia_cultivo_tipo.png", p_contingencia, width = 7, height = 4.8, dpi = 300)

provincia_cultivo_largo <- as.data.frame(as.table(pct_provincia_cultivo), stringsAsFactors = FALSE)
names(provincia_cultivo_largo) <- c("provincia", "cultivo", "porcentaje")
provincia_cultivo_largo <- provincia_cultivo_largo |>
  dplyr::filter(provincia %in% provincias_muestra, cultivo %in% cultivos_muestra)
p_provincia_cultivo <- ggplot2::ggplot(provincia_cultivo_largo,
  ggplot2::aes(x = cultivo, y = provincia, fill = porcentaje)) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(format(round(porcentaje, 1), nsmall = 1), "%")), size = 3.7, colour = "#172B4D") +
  ggplot2::scale_x_discrete(labels = stats::setNames(etiquetas_cultivos_muestra, cultivos_muestra)) +
  ggplot2::scale_fill_gradient(low = "#DBEAFE", high = "#1D4ED8") +
  ggplot2::labs(title = "Porcentaje por fila: Provincia × Cultivo",
    subtitle = "Siete territorios y seis cultivos; denominador: todos los cultivos de cada provincia",
    x = "Cultivo (etiquetas abreviadas)", y = "Provincia o agrupación", fill = "% por provincia") + tema_color +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, size = 10),
    axis.text.y = ggplot2::element_text(size = 10),
    plot.title = ggplot2::element_text(size = 14),
    plot.subtitle = ggplot2::element_text(size = 10),
    plot.margin = ggplot2::margin(10, 12, 10, 12)
  )
 ggplot2::ggsave("salidas_proyecto/12_contingencia_provincia_cultivo_pct_fila.png", p_provincia_cultivo, width = 9.4, height = 5.0, dpi = 300)

# El gráfico principal de correlación utiliza la base completa y las unidades originales,
# para coincidir con el diagrama de dispersión trabajado en clase. No se aplica logaritmo.
plot_cor <- data.frame(
  x = datos_cor_total$superficie_cosechada,
  y = datos_cor_total$produccion
)
p_correlacion <- ggplot2::ggplot(plot_cor, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_point(colour = "#2563EB", alpha = 0.42, size = 1.35) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                       colour = "#DC2626", linewidth = 0.9) +
  ggplot2::labs(title = "Superficie cosechada vs. producción (base completa)",
    subtitle = "Escalas originales; recta de regresión lineal",
    x = "Superficie cosechada (ha)", y = "Producción (toneladas)") + tema_color
 ggplot2::ggsave("salidas_proyecto/13_correlacion_base_completa.png", p_correlacion, width = 6.7, height = 4.5, dpi = 300)

# 8. Exportar tablas y base normalizada para anexos y verificación.
utils::write.csv(datos, "salidas_proyecto/datos_depurados.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(auditoria_calidad, "salidas_proyecto/auditoria_calidad.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(auditoria_tokens, "salidas_proyecto/auditoria_marcadores.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(freq_cultivo, "salidas_proyecto/frecuencias_cultivo.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(freq_tipo, "salidas_proyecto/frecuencias_tipo.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(freq_anio, "salidas_proyecto/frecuencias_anio.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(freq_provincia, "salidas_proyecto/frecuencias_provincia.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(resumen_cuantitativas, "salidas_proyecto/resumen_cuantitativas.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(contingencia_conteos, "salidas_proyecto/contingencia_conteos_totales.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(contingencia_porcentajes, "salidas_proyecto/contingencia_porcentajes_fila.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(contingencia_porcentajes_columna, "salidas_proyecto/contingencia_porcentajes_columna.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(contingencia_porcentajes_total, "salidas_proyecto/contingencia_porcentajes_total_base.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(tabla_provincia_cultivo_conteos_completa, "salidas_proyecto/contingencia_provincia_cultivo_frecuencias_completas.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(pct_provincia_cultivo_completa, "salidas_proyecto/contingencia_provincia_cultivo_porcentaje_fila_completa.csv", row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(resumen_correlaciones, "salidas_proyecto/resumen_correlaciones.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\nAnálisis ejecutado.\n")
cat("Archivo:", normalizePath(archivo), "\n")
cat("Filas:", nrow(datos), " | Años:", min(datos$anio, na.rm = TRUE), "-", max(datos$anio, na.rm = TRUE), "\n")
cat("Cultivos:", dplyr::n_distinct(datos$cultivo), " | Provincias/agrupaciones:", dplyr::n_distinct(datos$provincia),
    " | Tipos después de normalizar:", dplyr::n_distinct(datos$tipo), "\n\n")
print(auditoria_calidad)
print(resumen_cuantitativas)
print(resumen_correlaciones)
cat("\nTablas y gráficos guardados en la carpeta 'salidas_proyecto'.\n")
