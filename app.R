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

# censo.mun = tidyr::gather(censo.mun, "Indicador", "Valor", 6:237)
censo.uf = tidyr::gather(censo.uf, "Indicador", "Valor", 4:235)
censo.br = tidyr::gather(censo.br, "Indicador", "Valor", 3:234)

load("geo.RData")
colnames(geo.uf)[1] = "UF"

###################################################################################################
# UI    ###########################################################################################
###################################################################################################
## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Censos Brasil UF"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      selectInput(input = "Indicador",
                  label = "Selecione o indicador:",
                  choices = unique(censo.uf$Indicador),
                  selected = ""
                  # selectize=FALSE
                  # selected = 1
      ),
      menuItem("Estados", tabName = "estados", icon = icon("")),
      menuItem("BRA", tabName = "widgets", icon = icon("th")),
      renderUI({
        HTML(paste('<b>Indicador:</b>', 
                   filter(censo.siglas, SIGLA == 
                            input$Indicador)[2], 
                   '<b><br> Nome Curto:</b>', filter(censo.siglas, SIGLA == 
                                                       input$Indicador)[3],
                   '<b><br> Definição:</b>', filter(censo.siglas, SIGLA == 
                                                      input$Indicador)[4]))
        
      })
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItem(tabName = "estados",
    fluidRow(
      box(title = "1991",
          leafletOutput("plot1", height = 300)),
      box(title = "2000",
          leafletOutput("plot2", height = 300)),
      box(title = "2010",
          leafletOutput("plot3", height = 300))
      )
    ))
  )


server <- function(input, output) {

    dados1 = reactive({
      req(input$Indicador)
      geo.uf %>%
        dplyr::left_join(filter(censo.uf, ANO == 1991, Indicador == 
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
        dplyr::left_join(filter(censo.uf, ANO == 2000, Indicador == 
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
        dplyr::left_join(filter(censo.uf, ANO == 2010, Indicador == 
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
    
}

shinyApp(ui, server)