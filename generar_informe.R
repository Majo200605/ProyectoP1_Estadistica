# Ejecutar este archivo desde RStudio para generar el informe PDF IEEE.
# Antes, conserve en la misma carpeta el Excel, el .Rmd y analisis_completo_color.R.

archivo_rmd <- "Informe_Proyecto_Integrador_P1_IEEE_final.Rmd"
if (!file.exists(archivo_rmd)) {
  stop("No encuentro el archivo .Rmd. Abra esta carpeta como directorio de trabajo en RStudio y vuelva a ejecutar.")
}

paquetes <- c("rmarkdown", "knitr", "kableExtra", "readxl", "dplyr", "tidyr", "ggplot2")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) {
  install.packages(faltan, repos = "https://cloud.r-project.org")
}

if (!nzchar(Sys.which("xelatex"))) {
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    install.packages("tinytex", repos = "https://cloud.r-project.org")
  }
  stop(paste(
    "No se detecta xelatex. Instale LaTeX una sola vez ejecutando en la consola:",
    "tinytex::install_tinytex()",
    "Luego reinicie RStudio y vuelva a ejecutar generar_informe.R."
  ))
}

rmarkdown::render(
  input = archivo_rmd,
  output_format = "pdf_document",
  clean = TRUE,
  envir = new.env(parent = globalenv())
)

cat("\nGeneración finalizada. El informe PDF y las salidas estadísticas están en el directorio del proyecto.\n")
