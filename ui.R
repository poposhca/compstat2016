library(shiny)
source("module.R")

shinyUI(fluidPage(
  
  titlePanel("Estadistica Computacional"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h2("Tareas", style="color:gray"),
      p(actionButton("finv","Funcion Inversa")),
      p(actionButton("acre","Aceptación Rechazo"))
    ),
    
    mainPanel(
      br(),
      #uiOutput("ui")
      #Prueba de modulos
      modui("mod")
    )
    
  )
))