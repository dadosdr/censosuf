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




# censo.mun <- data.table::fread("Atlas2013mun.csv")
censo.mun <- read_excel("Atlas2013.xlsx",  sheet = "MUN 91-00-10")
# censo.uf <- read_excel("Atlas2013.xlsx",  sheet = "UF 91-00-10")
# censo.br <- read_excel("Atlas2013.xlsx",  sheet = "BR 91-00-10")
censo.siglas <- read_excel("Atlas2013.xlsx", sheet = "Siglas")

censo.mun = tidyr::gather(censo.mun, "Indicador", "Valor", 6:237)
colnames(censo.mun)[4] = "code_muni"
censo.mun$Valor = as.numeric(censo.mun$Valor)
censo.mun91 = censo.mun %>% dplyr::filter(ANO == "1991")
censo.mun00 = censo.mun %>% dplyr::filter(ANO == "2000")
censo.mun10 = censo.mun %>% dplyr::filter(ANO == "2010")
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
                  choices = unique(censo.mun$Indicador),
                  selected = ""
                  # selectize=FALSE
                  # selected = 1
      ),
      menuItem("Estados", tabName = "estados", icon = icon("")),
      menuItem("BRA", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItem(tabName = "estados",
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    ))
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)