#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  ##################################################################
  #########################Dados do Participante####################
  ##################################################################
  source(file="ArquivosServer/EF1Colunas.R", local = T, encoding = "UTF-8")[1]
  source(file="ArquivosServer/EF2TabelasGraficos.R", local = T, encoding = "UTF-8")[1]
  source(file="ArquivosServer/EMTabelasGraficos.R", local = T, encoding = "UTF-8")[1]
  source(file="ArquivosServer/EBServer.R", local = T, encoding = "UTF-8")[1]





})
