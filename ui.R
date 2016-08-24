shinyUI(fluidPage(
  
  titlePanel("Estadistica Computacional"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h2("Tareas", style="color:red"),
      p("Funcion Inversa", style="color:blue")
    ),
    
    mainPanel(
      br(),
      numericInput("lmd", label = "Lambda: ", value = 1),
      numericInput("n", label = "Número de simulaciones: ", value = 1000),
      plotOutput("invgraph")
    )
    
  )
))