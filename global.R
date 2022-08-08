## Load packages
library("shiny")
library("shinyjs")
library("shinyBS")
library("shinydashboard")
library("shinyWidgets") # nicer inputs
library("shinythemes")
library("plotly")
library("tidyverse")
library("treemap")
library("stringr")
library("data.table")
options(DT.options = list(scrollY="600px",scrollX="300px",
                          pageLength = 100,
                          columnDefs = list(list(className = 'dt-center', targets = "_all"))))
##############################################################################

## Define font to be used later
f1 = list(family = "Arial", size = 10, color = "rgb(30,30,30)")

## Function to format the dates for better plotting
printDate = function(date){
  # paste0(day(date),"/",month(date, lab=T, locale="us"))
  monthsEn=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  paste0(day(date),"/",monthsEn[month(date)])
}

## colors for observed data
blu = 'rgb(100,140,240)'
dblu = 'rgb(0,0,102)'
red = 'rgb(200,30,30)'
dred = 'rgb(100,30,30)'

##############################################################################
## DATA SOURCES

df1 <- readRDS("BancoDados/Transferencias.Rds")
df2 <- readRDS("BancoDados/Enem.Rds")
df3 <- readRDS("BancoDados/saeb.Rds")
Pop <- readRDS("BancoDados/PopEstadoMUN.Rds")
