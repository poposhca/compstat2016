library(Rcpp)
sourceCpp("mc.cpp")

mcui <- function(id)
{
  ns <- NS(id)
  tagList(
    h1('Markov Chain Monte Carlo'),
    fileInput(ns('file1'), label = 'Seleccionar archivo csv', accept = c('text/csv','text/comma-separated-values')),
    tableOutput(ns('data')),
    fluidRow(
      column(6,numericInput(ns('init'), label = 'Estado incial de simulacion:', value = 1)),
      column(6,numericInput(ns('nsim'), label = 'Simulaciones:', value = 5))
    ),
    h3('Simulaciones:'),
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
      as.matrix(read.csv(f$datapath))
  })
  
  output$data <- renderTable({
   data()
  })
  
  output$txt <- renderText({
    if(is.null(data()))
      return()
    mc_trayectoria(input$init,input$nsim,data())
  })
}