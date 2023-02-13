#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import tmap
#' @import dplyr
#' @import leaflet
#' @import htmltools
#' @import shinydashboard
#' @import plotly
#' @import ggthemes
#' @import gganimate
#' @import dplyr
#' @import flexdashboard
#' @import plotly
#' @import htmltools
#' @import readxl
#' @import geobr
#' @import tmap
#' @import geobr
#' @import RColorBrewer
#' @import shiny
#' @import leaflet
#' @import sf
#' @import spdep
#' @import broom
#' @import spdep
#' @import spatialreg
#' @import rgdal
#' @import sphet
#' @import tseries
#' @import readxl
#' @import splm
#' @import geobr
#' @import ggplot2
#' @import tmap
#' @import rgeoda
#' @import ggplot2
#' @import tseries
#' @import dplyr
#' @importFrom dplyr %>%
source("funcoescred.R")

censo.uf <- readxl::read_excel("Atlas2013.xlsx",  sheet = "UF 91-00-10")
censo.br <- readxl::read_excel("Atlas2013.xlsx",  sheet = "BR 91-00-10")
censo.siglas <- readxl::read_excel("Atlas2013.xlsx",  sheet = "Siglas")
colnames(censo.br)[1]= "BRA"
censo.uf91 = censo.uf |> dplyr::filter(ANO == "1991")
censo.uf00 = censo.uf |> dplyr::filter(ANO == "2000")
censo.uf10 = censo.uf |> dplyr::filter(ANO == "2010")
censo.uf91 = tidyr::gather(censo.uf91, "Indicador", "Valor", 4:235)
censo.uf00 = tidyr::gather(censo.uf00, "Indicador", "Valor", 4:235)
censo.uf10 = tidyr::gather(censo.uf10, "Indicador", "Valor", 4:235)
censo.br2 = tidyr::gather(censo.br, "Indicador", "Valor", 3:234)
censo.brvar = cbind(censo.uf91, censo.uf10)
colnames(censo.brvar) = c("ANO91", "UF", "UFN91", "Indicador91", "Valor91","ANO910", "UF10","UFN10", "Indicador10", "Valor10")
censo.brvar$Var = round(censo.brvar$Valor10 - censo.brvar$Valor91,1)

load("geo.RData")
colnames(geo.uf)[1] = "UF"


app_ui <- function(request) {

  dashboardPage( skin = "green",
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
                                      box(title = "1991",
                                          leafletOutput("plot1", height = 300)),
                                      box(title = "2000",
                                          leafletOutput("plot2", height = 300)),
                                      box(title = "2010",
                                          leafletOutput("plot3", height = 300)),
                                      box(title = "Variação 1991-2010",
                                          leafletOutput("plot4", height = 300)),
                                      box(title = "Descrição",
                                          collapsible = TRUE,
                                          collapsed=TRUE,
                                          # width=5,
                                          htmlOutput("desc")))),
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
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "censosuf"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
