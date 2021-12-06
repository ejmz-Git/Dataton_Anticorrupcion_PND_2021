

library(shiny)
library(echarts4r)
library(echarts4r.maps)
library(reactable)
library(dplyr)
library(leaflet)
library(scales)
library(data.table)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinyWidgets)
library(viridisLite)
#library(sever)

geoespacial <-  sf::read_sf("shapefiles/mexico/dest_2015gw.shp")
datos <- data.table::fread("datos/base_corrupcion.csv")
datos <- datos |> 
  group_by(`Nombre completo`) |> 
  arrange(Fecha, .by_group = TRUE)

datos$Estado[datos$Estado == "Coahuila de Zaragoza"] <- "Coahuila"
datos$Estado[datos$Estado == "Baja California Sur"] <- "South Lower California"
datos$Estado[datos$Estado == "San Luis Potosí"] <- "San Luis Potosi"
datos$Estado[datos$Estado == "Michoacán de Ocampo"] <- "Michoacán"
datos$Estado[datos$Estado == "Veracruz de Ignacio de la Llave"] <- "Veracruz"
datos$Estado[datos$Estado == "Ciudad de México"] <- "Federal District"
datos$Estado[datos$Estado == "México"] <- "Mexico"

data("cities")
df_coords <- cities |> 
  filter(country == "MX") |> 
  left_join(datos, by = c("city" = "Municipio"))

geoespacial$NOM_ENT[geoespacial$NOM_ENT == "Coahuila de Zaragoza"] <- "Coahuila"
geoespacial$NOM_ENT[geoespacial$NOM_ENT == "Baja California Sur"] <- "South Lower California"
geoespacial$NOM_ENT[geoespacial$NOM_ENT == "San Luis Potosí"] <- "San Luis Potosi"
geoespacial$NOM_ENT[geoespacial$NOM_ENT == "Michoacán de Ocampo"] <- "Michoacán"
geoespacial$NOM_ENT[geoespacial$NOM_ENT == "Veracruz de Ignacio de la Llave"] <- "Veracruz"
geoespacial$NOM_ENT[geoespacial$NOM_ENT == "Distrito Federal"] <- "Federal District"
geoespacial$NOM_ENT[geoespacial$NOM_ENT == "México"] <- "Mexico"


opciones_est <- geoespacial$NOM_ENT

df_coords <- df_coords |> 
  mutate(year = as.numeric(substr(Fecha,1,4)))

df_coords_filter <- df_coords |> 
  filter(year == 2018)


atipicos <- datos
atipicos <-atipicos |> 
  group_by(`Nombre completo`) |> 
  mutate(laggeado = (Ingreso-lag(Ingreso))/lag(Ingreso)) |> 
  #select(laggeado, Ingreso) |> 
  ungroup() |> 
  mutate(observacion =
           case_when(
             laggeado >= 3.5 ~ "Valor atípico",
             laggeado < 3.5 ~ "Valor normal"
           )
  ) |> 
  group_by(`Nombre completo`) |> 
  filter(any(observacion == "Valor atípico")) |> 
  ungroup()

atipicos$Fecha <- as.character(atipicos$Fecha)


max(atipicos$laggeado, na.rm = TRUE)





