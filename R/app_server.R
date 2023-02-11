#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  dados1 = reactive({
    req(input$Indicador)
    geo.uf %>%
      dplyr::left_join(filter(censo.uf91, Indicador ==
                                input$Indicador) , by = "UF")})

  output$plot1 = renderLeaflet({
    library(tmap)
    tmap_leaflet(
      plotmapakn(base = dados1(),
                 var = "Valor",
                 titulo = unique(input$Indicador),
                 estados = geo.uf,
                 Id = "UFN",
                 cores = "Spectral",
                 Style = "kmeans",
                 label = "Valor"))})

  dados2 = reactive({
    req(input$Indicador)
    geo.uf %>%
      dplyr::left_join(filter(censo.uf00, Indicador ==
                                input$Indicador) , by = "UF")})

  output$plot2 = renderLeaflet({
    library(tmap)
    tmap_leaflet(
      plotmapakn(base = dados2(),
                 var = "Valor",
                 titulo = unique(input$Indicador),
                 estados = geo.uf,
                 Id = "UFN",
                 cores = "Spectral",
                 Style = "kmeans",
                 label = "Valor"))})

  dados3 = reactive({
    req(input$Indicador)
    geo.uf %>%
      dplyr::left_join(filter(censo.uf10, Indicador ==
                                input$Indicador) , by = "UF")})

  output$plot3 = renderLeaflet({
    library(tmap)
    tmap_leaflet(
      plotmapakn(base = dados3(),
                 var = "Valor",
                 titulo = unique(input$Indicador),
                 estados = geo.uf,
                 Id = "UFN",
                 cores = "Spectral",
                 Style = "kmeans",
                 label = "Valor"))})



  output$desc = renderText({
    req(input$Indicador)
    HTML(paste('<b>Indicador:</b>',
               filter(censo.siglas, SIGLA ==
                        input$Indicador)[2],
               '<b><br> Nome Curto:</b>', filter(censo.siglas, SIGLA ==
                                                   input$Indicador)[3],
               '<b><br> Definição:</b>', filter(censo.siglas, SIGLA ==
                                                  input$Indicador)[4]))})
  output$desc2 = renderText({
    req(input$Indicador)
    HTML(paste('<b>Indicador:</b>',
               filter(censo.siglas, SIGLA ==
                        input$Indicador)[2],
               '<b><br> Nome Curto:</b>', filter(censo.siglas, SIGLA ==
                                                   input$Indicador)[3],
               '<b><br> Definição:</b>', filter(censo.siglas, SIGLA ==
                                                  input$Indicador)[4]))})

  dados4 = reactive({
    req(input$Indicador)
    geo.uf %>%
      dplyr::left_join(filter(censo.brvar, Indicador10 ==
                                input$Indicador) , by = "UF")

  })

  output$plot4 = renderLeaflet({
    library(tmap)
    tmap_leaflet(
      plotmapakn(base = dados4(),
                 var = "Var",
                 titulo = unique(input$Indicador),
                 estados = geo.uf,
                 Id = "UFN",
                 cores = "Spectral",
                 Style = "kmeans",
                 label = "Var"))})

  dados5 = reactive({
    req(input$Indicador)
    filter(censo.br2, Indicador == input$Indicador)

  })


  output$grafico <- renderPlot({
    library(ggplot2)
    req(input$Indicador)
    ggplot(dados5(), aes(as.factor(ANO), Valor)) +
      geom_col()+
      labs(x="ANO", title = input$Indicador)+
      geom_text(aes(label=Valor, vjust=2),color="white", size=10)
  })


}
