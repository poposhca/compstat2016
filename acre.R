acreui <- function(){
  ui <- list(
    h1("Simulación Aceptación Rechazo"),
    p("Funciones g(x) y f(x)"),
    textInput("fx","f(x):", value = "x" ),
    sliderInput("m","M:", 1, 100, 1),
    plotOutput("plt")
  )
  return (ui)
}

gx <- reactive({
  function(x) 1*(x>0 && x<1)
})