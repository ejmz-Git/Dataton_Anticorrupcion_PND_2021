# Instala las dependencias directas de la aplicación.
# Ejecutar desde la carpeta raíz: Rscript install.R

packages <- c(
  "shiny",
  "echarts4r",
  "echarts4r.maps",
  "reactable",
  "dplyr",
  "leaflet",
  "scales",
  "data.table",
  "RColorBrewer",
  "shinyWidgets",
  "viridisLite",
  "sf"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

message("Dependencias instaladas o ya disponibles.")