library(shiny)
source("finvmodule.R")
source("intmodule.R")
source("acremodule.R")
source("bigmodule.R")
source("mcmodule.R")
source("mhmodule.R")

shinyUI(fluidPage(
  
  titlePanel("Estadistica Computacional"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h2("Tareas", style="color:gray"),
      p(actionLink("finv","1) Funcion Inversa")),
      p(actionLink("integral","2) Integral definida")),
      p(actionLink("mh", "3) Metropolis Hasting")),
      br(),
      h2("Otros ejercicios", style="color:gray"),
      p(actionLink("acre","Aceptacion Rechazo")),
      p(actionLink("big","Ley de los grandes numeros")),
      p(actionLink("mc","MCMC"),"(Primera version)")
    ),
    
    mainPanel(
      br(),
      uiOutput("ui")
    )
    
  )
))