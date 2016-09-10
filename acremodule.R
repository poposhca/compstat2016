library(shiny)

acreui <- function(id)
{
  ns <- NS(id)
  tagList(
    h1("Simulación Aceptación Rechazo"),
    p("Funciones g(x) y f(x)"),
    textInput(ns("fx"),"f(x):", value = "x" ),
    sliderInput(ns("m"),"M:", 1, 100, 1),
    plotOutput(ns("plt"))
  )
}

acreserver <- function(input, output, session)
{
  gx <- reactive({
    function(x) 1*(x>0 && x<1)
  })
  
  output$plt <- renderPlot({
    x <- seq(0, 1, length.out=100)
    y1 = input$m*sapply(x,gx())
    y2 = sapply(x, fx())
    plot_limit = c(min(c(y1, y2)), max(c(y1, y2)))
    plot(x,y1, ylim = plot_limit)
    lines(x,y2)
  })
  
  fx <- reactive({
    f <- paste("aux <- function(x) ", input$fx)
    eval(parse(text = f))
    aux
  })
  
}