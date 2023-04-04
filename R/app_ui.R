#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import plotly
#' @import ggthemes
#' @import gganimate
#' @import dplyr
#' @import flexdashboard
#' @import plotly
#' @import htmltools
#' @import geobr
#' @import RColorBrewer
#' @import leaflet
#' @import sf
# @import spdep
# @import broom
# @import spdep
# @import spatialreg
# @import rgdal
# @import sphet
# @import readxl
# @import splm
#' @import ggplot2
#' @import tmap
#' @import DT
#' @importFrom DT dataTableOutput datatable
# @import rgeoda
# @import tseries
#' @importFrom dplyr %>%
#' @importFrom shinydashboard valueBox
#source("funcoescred.R")
library(shinydashboard)


#devtools::load_all(".")
#devtools::build()

plotmapakn = function(..., base,
                      var,
                      Id,
                      titulo,
                      label,
                      estados,
                      cores, Style = c("kmeans","sd","jenks","hclust","bclust"))  {
  {

    # tab = table(ifelse(base$var2<=1,"0 a 1",
    #                    ifelse(base$var2<=2,"1 a 2", "2 ou maior")))

    library(tmap)

    tm_shape(base) + tm_fill(var,
                             # breaks = c(0,22642931 , 595701218, Inf),
                             style = Style, #"kmeans", # kmeans, sd, fisher, jenks, hclust, bclust, quantile
                             #breaks = Breaks,
                             # breaks=c(0,quantile(paste(base,"$",var,"[",base,"$",var, ">0]"), na.rm=TRUE)), textNA = "Zero",
                             colorNA = "gray",
                             legend.hist = F,
                             id = Id,
                             palette=cores)+
      tm_text(label) +
      # tm_compass(position=c("right", "bottom"))+
      tm_layout(frame = FALSE, main.title = titulo, asp=0, title.position = c('right', 'top'))+
      tm_borders("black", lwd=.1, alpha = .3)+
      # tm_scale_bar(width = 0.22, position=c("right", "bottom"),lwd = .2)+
      tm_legend(position=c("left", "bottom"),
                # compass.type="arrow",
                compass.type="8star",
                # legend.outside = F,
                legend.format = list(text.separator= "a",
                                     text.or.more="ou maior",
                                     text.less.than="menor que"
                                     ,fun=function(x) formatC(x,
                                                              big.mark = ".",
                                                              decimal.mark = ",",
                                                              digits=2,
                                                              format="fg"
                                     )))+
      tm_shape(estados)+
      tm_borders("black", lwd=1)

  }}

censo.uf <- readxl::read_excel("Atlas2013.xlsx",  sheet = "UF 91-00-10")
censo.br <- readxl::read_excel("Atlas2013.xlsx",  sheet = "BR 91-00-10")
censo.siglas <- readxl::read_excel("Atlas2013.xlsx",  sheet = "Siglas")
colnames(censo.br)[1]= "BRA"
censo.uf91 = censo.uf |> dplyr::filter(ANO == "1991")
censo.uf00 = censo.uf |> dplyr::filter(ANO == "2000")
censo.uf10 = censo.uf |> dplyr::filter(ANO == "2010")
censo.uf91 = tidyr::gather(censo.uf91, "Indicador", "Valor", 4:235)
censo.uf91 =  censo.uf91 %>% dplyr::left_join(censo.siglas, by=c("Indicador" = "SIGLA"))
censo.uf00 = tidyr::gather(censo.uf00, "Indicador", "Valor", 4:235)
censo.uf00 =  censo.uf00 %>% dplyr::left_join(censo.siglas, by=c("Indicador" = "SIGLA"))
censo.uf10 = tidyr::gather(censo.uf10, "Indicador", "Valor", 4:235)
censo.uf10 =  censo.uf10 %>% dplyr::left_join(censo.siglas, by=c("Indicador" = "SIGLA"))
censo.br2 = tidyr::gather(censo.br, "Indicador", "Valor", 3:234)
censo.br2 =  censo.br2 %>% dplyr::left_join(censo.siglas, by=c("Indicador" = "SIGLA"))
censo.brvar = cbind(censo.uf91, censo.uf10)
colnames(censo.brvar) = c("ANO91", "UF", "UFN91", "Indicador91", "Valor91",  "NOME CURTO91",  "NOME LONGO91", "DEFINIÇÃO91",
                          "ANO910", "UF10","UFN10", "Indicador10", "Valor10", "NOME CURTO",  "NOME LONGO", "DEFINIÇÃO")
censo.brvar$Var = round(censo.brvar$Valor10 - censo.brvar$Valor91,2)


load("geo2.RData")
colnames(geo.uf)[1] = "UF"
gc(reset=TRUE)

app_ui <- function(request) {

  dashboardPage( skin = "green",
                 dashboardHeader(title = "DadosDR - Censos (Atlas Brasil)",titleWidth = 400),
                 ## Sidebar content
                 dashboardSidebar(
                   sidebarMenu(
                     selectInput(input = "Indicador",
                                 label = "Selecione o indicador:",
                                 choices = censo.siglas$`NOME CURTO`[-c(1:5)],
                                 # selected = censo.siglas$`NOME CURTO`[6]
                                  selectize=FALSE
                                 # selected = 1
                     ),
                     menuItem("Estados", tabName = "estados", icon = icon("th")),
                     menuItem("BRA", tabName = "bra", icon = icon("flag")),
                     menuItem("MUN", tabName = "mun", icon = icon("map-location")),
                     menuItem("Dados", tabName = "plans", icon = icon("database")),
                     menuItem("Sobre", tabName = "sobre", icon = icon("users"))

                   )
                 ),
                 dashboardBody(
                   # Boxes need to be put in a row (or column)
                   tabItems(tabItem(tabName = "estados",
                                    fluidRow(
                                      box(title = "Descrição",
                                          collapsible = TRUE,
                                          collapsed=FALSE,
                                          width=12,
                                          htmlOutput("desc")),
                                      box(title = "1991",
                                          tmapOutput("plot1", height = 300)),
                                      box(title = "2000",
                                          tmapOutput("plot2", height = 300)),
                                      box(title = "2010",
                                          tmapOutput("plot3", height = 300)),
                                      box(title = "Variação 1991-2010 (unitária)",
                                          tmapOutput("plot4", height = 300)),
                                      )),
                            tabItem(tabName = "bra",
                                    fluidRow(
                                      box(title = "Gráfico", plotOutput("grafico")),
                                      box(title = "Descrição",htmlOutput("desc2"))
                                    )
                            ),
                            tabItem(tabName = "plans",
                                    fluidRow(
                                      column(width = 12,
                                      box(title="BRA", DT::dataTableOutput("tabBR", width = 500),collapsible = TRUE,
                                          collapsed=TRUE),
                                      box(title="UF",  DT::dataTableOutput("tabbr", width = 500),collapsible = TRUE,
                                          collapsed=TRUE),
                                      box(title="MUN", "Dados dos municípios",collapsible = TRUE,
                                          collapsed=TRUE))

                                    )),
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
