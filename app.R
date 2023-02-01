
# Carga de paquetes ----
library(shiny)
library(tmap)
library(dplyr)
library(leaflet)
library(htmltools)
library(shinydashboard)
library(plotly)
library(ggthemes)
library(gganimate)
library(dplyr)
# gc(reset=TRUE)
# htmltools::includeHTML("google-analytics.html")
source("funcoescred.R")
library(flexdashboard)
library(plotly)
library(htmltools)
library(readxl)
library(geobr)
library(tmap)
library(geobr)
library(RColorBrewer)
library(shiny)
library(leaflet)
library(sf)
library(spdep)
library(broom)
library(spdep)
library(spatialreg)
library(rgdal)
library(sphet)
library(tseries)
library(readxl)
library(splm)
library(geobr)
library(ggplot2)
library(tmap)
library(rgeoda)
library(ggplot2)
library(tseries)
library(dplyr)




# censo.mun <- read_excel("Atlas2013.xlsx",  sheet = "MUN 91-00-10")
censo.uf <- read_excel("Atlas2013.xlsx",  sheet = "UF 91-00-10")
censo.br <- read_excel("Atlas2013.xlsx",  sheet = "BR 91-00-10")
censo.siglas <- read_excel("Atlas2013.xlsx",  sheet = "Siglas")
colnames(censo.br)[1]= "BRA"

# censo.mun = tidyr::gather(censo.mun, "Indicador", "Valor", 6:237)
# censo.uf = tidyr::gather(censo.uf, "Indicador", "Valor", 4:235)
# censo.br = tidyr::gather(censo.br, "Indicador", "Valor", 3:234)
censo.uf91 = censo.uf %>% dplyr::filter(ANO == "1991")
censo.uf00 = censo.uf %>% dplyr::filter(ANO == "2000")
censo.uf10 = censo.uf %>% dplyr::filter(ANO == "2010")
censo.uf91 = tidyr::gather(censo.uf91, "Indicador", "Valor", 4:235)
censo.uf00 = tidyr::gather(censo.uf00, "Indicador", "Valor", 4:235)
censo.uf10 = tidyr::gather(censo.uf10, "Indicador", "Valor", 4:235)
censo.br2 = tidyr::gather(censo.br, "Indicador", "Valor", 3:234)


load("geo.RData")
colnames(geo.uf)[1] = "UF"

###################################################################################################
# UI    ###########################################################################################
###################################################################################################
## app.R ##
library(shinydashboard)

ui <- dashboardPage( skin = "green",
  dashboardHeader(title = "DadosDR - Censos Brasil UF",titleWidth = 300),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      selectInput(input = "Indicador",
                  label = "Selecione o indicador:",
                  choices = censo.siglas$SIGLA[-c(1:5)],
                  selected = "ESPVIDA"
                  # selectize=FALSE
                  # selected = 1
      ),
      menuItem("Estados", tabName = "estados", icon = icon("th")),
      menuItem("BRA", tabName = "bra", icon = icon("flag")),
      menuItem("Sobre", tabName = "sobre", icon = icon("users"))

    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(tabItem(tabName = "estados",
            fluidRow(
              box(title = "Descrição",
                  htmlOutput("desc")),
              box(title = "1991",
                  leafletOutput("plot1", height = 300)),
              box(title = "2000",
                  leafletOutput("plot2", height = 300)),
              box(title = "2010",
                  leafletOutput("plot3", height = 300)))),
            tabItem(tabName = "bra",
                    fluidRow(
                box(title = "Gráfico", plotOutput("grafico")),
                box(title = "Descrição",htmlOutput("desc2"))
                )
              
            ),
            tabItem(tabName = "sobre",
                    fluidRow(
                      box(title = "Autores",
                          p(strong("Felipe Micail da Silva Smolski"),
                          em(a("- Lattes", href="http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K8279859Z3"))),
                          p(strong("Reneo Prediger"), em(a("- Lattes", href="http://lattes.cnpq.br/7326249072472320"))),
                          p(strong("Edemar Rotta"), em(a("- Lattes", href="http://lattes.cnpq.br/966111258493392"))),
                          p(a("DadosDr", href="https://dadosdr.uffs.edu.br/")),
                          collapsible = TRUE,
                          collapsed=FALSE),
                      box(title = "Fonte",
                          p("Atlas Brasil"),
                          collapsible = TRUE,
                          collapsed=FALSE)
                      ))
            
  )
))



server <- function(input, output) {

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
    
   dados4 =  reactive({
     req(input$Indicador)
     filter(censo.br2, Indicador ==
             input$Indicador)})
     
output$grafico <- renderPlot({
  library(ggplot2)
  # req(input$Indicador)
ggplot(dados4(), aes(as.factor(ANO), Valor)) +
    geom_col()+
    labs(x="ANO")+
    geom_text(aes(label=Valor, vjust=2),color="white", size=10)
})



}

shinyApp(ui, server)