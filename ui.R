
ui <- htmlTemplate(filename = "www/index.html",
                   
                   mapas_gen = fluidPage(
                     column(
                       width = 12,
                       align=" center",
                       echarts4rOutput("barras", height = 290, width = 1100)
                     ),
                     column(width = 6,
                            align="center",
                            tags$div(
                              style = "margin-left: -75px", offset = 0,
                            echarts4rOutput("pie", width = 600, height = 350))
                            ),
                     column(width = 6,
                            align="center",
                            tags$div(
                              style = "margin-left: -75px", offset = 0,
                            echarts4rOutput("mapa", width = 600, height = 350
                                            ))
                            )
                   ),
                   
                   dinamico = fluidPage(
                     column(width = 12,
                            div(style="display: inline-block;vertical-align:top; width: 350px;",
                                selectInput(
                                  "opciones", "Escoge un estado:", choices = c("Todos",opciones_est)
                                )),
                            div(style="display: inline-block;vertical-align:top; width: 350px;",
                                selectInput(
                                  "civil", "Escoge un estado civil:", choices = NULL
                                )),
                            div(style="display: inline-block;vertical-align:top; width: 240px;",
                                #selectInput("periodo", "año", choices = c(2016,2017,2018,2019,2020))
                                prettyRadioButtons(
                                  inputId = "periodo",
                                  label = "Escoge un año:", 
                                  inline = TRUE,
                                  choices = c(2016,2017,2018,2019,2020)
                                )
                            )),
                   column(width = 5,
                          br(), br(),
                          
                          echarts4rOutput("dinamico_1", height = 300),
                          br(), br(),br(), br(),br(), 
                          echarts4rOutput("dinamico_2", height = 300)
                          
                          ),
                   column(width = 7,
                          leafletOutput("mapa_1", width = 700, height = 800)
                          )
                     
                   ),
                   selector_fin = selectInput("persona", "Escoge a un contribuyente:",
                                              unique(atipicos$`Nombre completo`)
                                              ),
                   atips = echarts4rOutput("grafico_atipico"),
                   
                   texto_fin = textOutput("texto_rend")
                   
                   
                   
                   
                   
                )
                   


