library(shiny)
source("finvmodule.R")
source("acremodule.R")

shinyUI(fluidPage(
  
  titlePanel("Estadistica Computacional"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h2("Tareas", style="color:gray"),
      p(actionLink("finv","Funcion Inversa")),
      p(actionLink("acre","Aceptación Rechazo"))
    ),
    
    mainPanel(
      br(),
      uiOutput("ui")
    )
    
  )
))