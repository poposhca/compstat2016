library(Rcpp)
sourceCpp("mc.cpp")

mcui <- function(id)
{
  ns <- NS(id)
  tagList(
    h1('Markov Chain Monte Carlo'),
    fileInput(ns('file1'), label = 'Seleccionar archivo csv', accept = c('text/csv','text/comma-separated-values')),
    tableOutput(ns('data')),
    textOutput(ns('txt'))
  )
}

mcserver <- function(input, output, session)
{
  data <- reactive({
    f = input$file1
    if(is.null(f))
      NULL
    else
      read.csv(f$datapath)
  })
  
  output$data <- renderTable({
   data()
  })
  
  output$txt <- renderText({
    square(6)
  })
}