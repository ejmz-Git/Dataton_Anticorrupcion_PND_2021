# Datatón Anticorrupción PND 2021

Aplicación interactiva desarrollada por el equipo **Datapunks** para explorar datos de ingresos declarados por personas servidoras públicas e identificar variaciones que ameritan análisis.

🏆 **Primer lugar** en el Datatón Anticorrupción de la Ciudad de México, edición 2021.

> **Alcance:** los valores atípicos son señales exploratorias; por sí solos no prueban corrupción ni conducta indebida.

## Arquitectura

~~~mermaid
flowchart LR
  CSV["datos/base_corrupcion.csv"] --> GLOBAL["global.R<br/>Carga, preparación y detección exploratoria"]
  SHP["shapefiles/mexico/"] --> GLOBAL
  GLOBAL --> SERVER["server.R<br/>Filtros y lógica reactiva"]

  TEMPLATE["www/index.html<br/>Plantilla HTML, estilos y recursos"] --> UI["ui.R<br/>Interfaz y controles"]
  UI --> APP["Aplicación R Shiny"]
  SERVER --> APP

  APP --> CHARTS["Gráficos interactivos<br/>echarts4r"]
  APP --> MAPS["Mapas<br/>Leaflet"]
  APP --> SUMMARY["Resumen de variaciones atípicas"]

  APP --> RUNTIME["Ejecución local o publicación Shiny"]
  RUNTIME --> BROWSER["Navegador"]
~~~
## ¿Qué incluye?

- Resúmenes de ingresos por universidad y entidad.
- Exploración por año, estado y estado civil, con gráficos y mapa.
- Comparación de ingresos por persona e institución.
- Detección exploratoria de cambios de ingreso respecto del registro anterior.

La aplicación está construida con **R Shiny**, gráficos de **echarts4r** y mapas con **Leaflet**. El código de la app se organiza en los archivos ui.R, server.R y global.R.

## Ejecutar localmente

Requisitos: **R 4.1 o posterior**. Desde la carpeta del proyecto, en una terminal:

1. Instala los paquetes del proyecto:

   ~~~sh
   Rscript install.R
   ~~~

2. Inicia la aplicación:

   ~~~sh
   Rscript -e "shiny::runApp()"
   ~~~

También puedes abrir **Dataton corrupcion.Rproj** en RStudio y ejecutar la app desde allí.

## Estructura

- ui.R, server.R, global.R: interfaz, lógica reactiva y preparación de datos.
- datos/base_corrupcion.csv: datos usados por la aplicación.
- shapefiles/mexico/: geometrías para el análisis geográfico.
- www/: plantilla visual, estilos, imágenes y recursos web.
- install.R: instalación de dependencias de R.

## Datos y uso responsable

La detección se basa en variaciones entre registros consecutivos de ingreso y debe interpretarse como exploración, no como conclusión. Los datos incluidos son sintéticos y fueron producidos por el equipo de Software Guru y el equipo organizador del Datatón para efectos de la competencia. Aunque son sintéticos, el CSV contiene campos etiquetados como nombre y CURP; consulta las condiciones de la competencia antes de reutilizar o redistribuir la base.

## Aplicación publicada

La versión en línea indicada originalmente por el equipo está en [shinyapps.io](https://jorgehcas1998.shinyapps.io/Dataton-corrupcion/#data). Su actualización depende de la configuración de despliegue de esa instancia.