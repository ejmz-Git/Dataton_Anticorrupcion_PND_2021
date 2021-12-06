
server <- function(input, output, session) {
  
  
  output$barras <- renderEcharts4r({
    
    datos |> 
      group_by(Universidad) |> 
      summarise(total = mean(Ingreso)) |> 
      arrange(desc(total)) |> 
      e_charts(Universidad) |> 
      e_bar(total) |> 
      #e_tooltip(trigger = "axis") |> 
      e_theme("auritus") |> 
      e_color(color = "#8a3732") |> 
      e_tooltip(trigger = "axis",
                confine = TRUE,
                formatter = e_tooltip_pointer_formatter("currency",
                                                        digits = 2),
                textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |>
      e_title("Ingresos por universidad de procedencia", 
              "Un vistazo general",
              left = "center",
              textStyle = list(
                color = "gray",
                fontFamily = "Roboto Condensed"
              )
      ) |> 
      e_legend(FALSE) |> 
      e_y_axis(show = FALSE)
    
  })
  
  output$pie <- renderEcharts4r({
    
    datos |> 
      group_by(`Estado Civil`) |> 
      summarise(total = n()) |> 
      e_charts(`Estado Civil`) |> 
      e_pie(total,radius = c("50%", "70%"),
            label = list(show = FALSE), name = "Participación") |>
      e_title("Proporción de los contribuyentes", 
              "Por estado civil",
              left = "center",
              textStyle = list(
                color = "gray",
                fontFamily = "Roboto Condensed"
              )
      ) |> 
      e_color(color = RColorBrewer::brewer.pal(11, "RdGy")) |> 
      e_tooltip(trigger = "item",
                confine = TRUE,
                textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |> 
      e_legend(FALSE)
    
  })
  
  output$mapa <- renderEcharts4r({
    
    datos |> 
      group_by(Estado) |> 
      summarise(total = mean(Ingreso)) |> 
      e_charts(Estado) |> 
      em_map("Mexico") |> 
      e_map(total, map = "Mexico") |> 
      e_visual_map(total, show = FALSE,
                   inRange = list(color = c("#949494", "#803030")) ) |> 
      e_tooltip(trigger = "item") |> 
      e_theme("auritus") |> 
      e_tooltip(trigger = "item",
                confine = TRUE,
                formatter = e_tooltip_choro_formatter(style = "currency"),
                textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |>
      e_title("¿Qué estado tiene el ingreso más alto?", 
              "Un vistazo a ingresos promedio",
              left = "center",
              textStyle = list(
                color = "gray",
                fontFamily = "Roboto Condensed"
              )
      )
    
    
  })
  
  
  output$mapa_1 <- renderLeaflet({
    
    paleta_0 <- colorBin( palette="Reds", domain=df_coords_filter$Ingreso, na.color="transparent", bins=5)
    
    leaflet(df_coords_filter$Ingreso) |> 
      addTiles() |> 
      addProviderTiles(provider = providers$CartoDB.Positron) |> 
      setView(-102.5528,23.6345, zoom = 5) |> 
      addCircleMarkers(~lon, ~lat, 
                       data = df_coords_filter,
                       fillColor = ~paleta_0(Ingreso), 
                       fillOpacity = 0.9, color="white", radius=~Ingreso^(1/9), 
                       stroke=FALSE,
                       #label = mytext,
                       labelOptions = 
                         labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", 
                                       direction = "auto"))
    
  })
  
  datos_filt <- reactive({
    
    if (input$opciones == "Todos"){
      df_coords |> 
        filter(year == input$periodo) 
    }else{
      df_coords |> 
        filter(year == input$periodo 
               & Estado == input$opciones
               
        ) |> 
        na.omit()
    }
    
    
  })
  
  bandas <- reactive({
    
    if (input$opciones == "Todos"){
      geoespacial |> 
        #filter(NOM_ENT == input$opciones ) |> 
        sf::st_bbox(geoespacial$geometry)  |> 
        as.vector()
    }else{
      geoespacial |> 
        filter(NOM_ENT == input$opciones ) |> 
        sf::st_bbox(geoespacial$geometry)  |> 
        as.vector()
    }
    
  })
  
  
  datos_selector <- reactive({
    
    if (input$opciones == "Todos"){
      
      df_coords |> 
        group_by(`Estado Civil`) |> 
        summarise(n())
    }else{
      df_coords |> 
        filter(Estado == input$opciones
               
        ) |> 
        group_by(`Estado Civil`) |> 
        summarise(n())
    }
    
  })
  
  
  observe({
    
    req(datos_selector())
    
    updateSelectInput(session,"civil", "Escoge un estado civil:", choices = datos_selector()$`Estado Civil` )
    
  })
  
  
  
  observe({
    paleta_1 <- colorBin( palette="Reds", domain=datos_filt()$Ingreso, na.color="transparent", bins=5)
    
    leafletProxy("mapa_1", data = datos_filt()) |> 
      #clearShapes() |> 
      clearControls() |> 
      clearMarkers() |> 
      addCircleMarkers(~datos_filt()$lon, ~datos_filt()$lat, 
                       data = df_coords_filter,
                       fillColor = ~paleta_1(Ingreso), 
                       fillOpacity = 0.9, color="white", radius=~datos_filt()$Ingreso^(1/9), 
                       stroke=FALSE,
                       #label = mytext,
                       labelOptions = 
                         labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", 
                                       direction = "auto")) |> 
      flyToBounds(
        bandas()[1], bandas()[2],bandas()[3],
        bandas()[4]
      )
    
  })
  
  output$dinamico_1 <- renderEcharts4r({
    
    if (input$opciones == "Todos"){
      df_coords |> 
        filter(year == input$periodo & `Estado Civil` == input$civil ) |> 
        group_by(`Nombre completo`) |> 
        summarise(total = mean(Ingreso)) |> 
        arrange(desc(total)) |> 
        top_n(10,total) |> 
        e_charts(`Nombre completo`, dispose = FALSE) |> 
        e_bar(total) |> 
        e_theme("auritus") |> 
        e_color(color = "#8a3732") |> 
        e_tooltip(trigger = "axis",
                  confine = TRUE,
                  formatter = e_tooltip_pointer_formatter("currency",
                                                          digits = 2),
                  textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |>
        e_title("Ingresos promedio", 
                "10 principales personas con mayor ingreso",
                left = "center",
                textStyle = list(
                  color = "gray",
                  fontFamily = "Roboto Condensed"
                )
        ) |> 
        e_legend(FALSE) |> 
        e_y_axis(show = FALSE)
    }else{
      df_coords |> 
        filter(Estado == input$opciones
               & year == input$periodo & 
                 `Estado Civil` == input$civil
               
               ) |> 
        group_by(`Nombre completo`) |> 
        summarise(total = mean(Ingreso)) |> 
        arrange(desc(total)) |> 
        top_n(10,total) |> 
        e_charts(`Nombre completo`, dispose = FALSE) |> 
        e_bar(total) |> 
        e_theme("auritus") |> 
        e_color(color = "#8a3732") |> 
        e_tooltip(trigger = "axis",
                  confine = TRUE,
                  formatter = e_tooltip_pointer_formatter("currency",
                                                          digits = 2),
                  textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |>
        e_title("Ingresos promedio", 
                "10 principales personas con mayor ingreso",
                left = "center",
                textStyle = list(
                  color = "gray",
                  fontFamily = "Roboto Condensed"
                )
        ) |> 
        e_legend(FALSE) |> 
        e_y_axis(show = FALSE)
    }
    
  })
  
  
  output$dinamico_2 <- renderEcharts4r({
    
    if (input$opciones == "Todos"){
      df_coords |> 
        filter(year == input$periodo & `Estado Civil` == input$civil ) |> 
        group_by(Institucion ) |> 
        summarise(total = mean(Ingreso)) |> 
        arrange(desc(total)) |> 
        top_n(10,total) |> 
        e_charts(Institucion, dispose = FALSE) |> 
        e_bar(total) |> 
        e_theme("auritus") |> 
        e_color(color = "#8a3732") |> 
        e_tooltip(trigger = "axis",
                  confine = TRUE,
                  formatter = e_tooltip_pointer_formatter("currency",
                                                          digits = 2),
                  textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |>
        e_title("Ingresos promedio", 
                "Agrupados por escuela de procedencia",
                left = "center",
                textStyle = list(
                  color = "gray",
                  fontFamily = "Roboto Condensed"
                )
        ) |> 
        e_legend(FALSE) |> 
        e_y_axis(show = FALSE)
    }else{
      df_coords |> 
        filter(Estado == input$opciones
               & year == input$periodo & 
                 `Estado Civil` == input$civil
               
        ) |> 
        group_by(Institucion) |> 
        summarise(total = mean(Ingreso)) |> 
        arrange(desc(total)) |> 
        top_n(10,total) |> 
        e_charts(Institucion, dispose = FALSE) |> 
        e_bar(total) |> 
        e_theme("auritus") |> 
        e_color(color = "#8a3732") |> 
        e_tooltip(trigger = "axis",
                  confine = TRUE,
                  formatter = e_tooltip_pointer_formatter("currency",
                                                          digits = 2),
                  textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |>
        e_title("Ingresos promedio", 
                "Agrupados por escuela de procedencia",
                left = "center",
                textStyle = list(
                  color = "gray",
                  fontFamily = "Roboto Condensed"
                )
        ) |> 
        e_legend(FALSE) |> 
        e_y_axis(show = FALSE)
    }
    
  })
  
  output$grafico_atipico <- renderEcharts4r({
    
    atipicos |> 
      filter(`Nombre completo` ==input$persona) |> 
      e_chart(Fecha, dispose = FALSE) |> 
      e_bar(Ingreso) |> 
      e_theme("auritus") |> 
      e_color(color = "#8a3732") |> 
      e_tooltip(trigger = "axis",
                confine = TRUE,
                formatter = e_tooltip_pointer_formatter("currency",
                                                        digits = 2),
                textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)) |>
      e_title("Los ingresos reportados", 
              paste0("De ", input$persona),
              left = "center",
              textStyle = list(
                color = "gray",
                fontFamily = "Roboto Condensed"
              )
      ) |> 
      e_legend(FALSE) |> 
      e_y_axis(show = FALSE)
    
  })
  
  output$texto_rend <- renderText({
    
    valor_text <- atipicos |> 
      filter(`Nombre completo` ==input$persona)
    
    paste0(
      "En este caso específico, tenemos que el contribuyente ",
      input$persona, ", quien trabaja en la dependencia '",
      head(valor_text$Institucion,1), "' y estudió en la universidad '",
      head(valor_text$Universidad,1),
      "' tuvo un ingreso máximo reportado de ",
      scales::comma(max(valor_text$Ingreso), prefix = "$"),
      ", siendo que, además, la variación porcentual entre ese ingreso máximo y el ingreso inmediato anterior fue del ",
      
      round(max(valor_text$laggeado, na.rm = TRUE), digits = 3), "%, lo cual hace sospechar que se trate de un caso de corrupción, al tener un incremento bastante considerable en sus ingresos." 
      
    )
    
    
  })
}
