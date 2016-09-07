invui <- function(){
  ui <- list(
    h1("Metodo de la función Inversa"),
    numericInput("lmd", label = "Lambda: ", value = 1),
    numericInput("n", label = "Número de simulaciones: ", value = 1000),
    plotOutput("invgraph"),
    p("Prueba de ajuste de bondad"),
    textOutput("chi")
  )
  return (ui)
}