library(shiny)

finvdui <- function(id)
{
  ns <- NS(id)
  tagList(
    h1("Metodo de la funcion Inversa"),
    numericInput(ns("lmd"), label = "Lambda: ", value = 1),
    numericInput(ns("n"), label = "Numero de simulaciones: ", value = 1000),
    numericInput(ns("nbin"), label = "Numero de cajones: ", value = 20, step = 5),
    plotOutput(ns("invgraph")),
    p("Prueba de ajuste de bondad"),
    textOutput(ns("chi"))
  )
}

fiserv <- function(input, output, session)
{
  GetV <- function(n){
    return (runif(n))
  }
  
  Finv <- function(u, lambda){
    return (-log(1-u)/lambda)
  }
  
  output$invgraph <- renderPlot({
    v <- GetV(input$n)
    f <- Finv(v,input$lmd)
    hist(f, breaks = input$nbin)
  })
  
  bondad <- function(u){
    chisq.test(u)
  }
  
  output$chi <- renderText({
    v <- GetV(input$n)
    f <- Finv(v,input$lmd)
    bondad(f)$p.value
  })
}