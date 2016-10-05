library(shiny)
source("finvmodule.R")
source("intmodule.R")
source("acremodule.R")
source("bigmodule.R")
source("mcmodule.R")

shinyUI(fluidPage(
  
  titlePanel("Estadistica Computacional"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h2("Tareas", style="color:gray"),
      p(actionLink("finv","1) Funcion Inversa")),
      p(actionLink("integral","2) Integral definida")),
      p(actionLink("mc", "3) Markov Chain")),
      br(),
      h2("Ejercicos de clase", style="color:gray"),
      p(actionLink("acre","Aceptacion Rechazo")),
      p(actionLink("big","Ley de los grandes numeros"))
    ),
    
    mainPanel(
      br(),
      uiOutput("ui")
    )
    
  )
))