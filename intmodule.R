intui <- function(id)
{
  ns <- NS(id)
  tagList(
    h2("Modulo de integral definida"),
    textInput(ns("fx"), label = "f(x)", value = "x"),
    fluidRow(
      column(4, numericInput(ns('min'), label = 'X min', value = 0)),
      column(4, numericInput(ns('max'), label = 'X max', value = 10)),
      column(4, numericInput(ns('eps'), label = 'eps', value = 0.5))
    ),
    selectInput(inputId = ns("met"), "Metodo de integracion: ", 
                choices = c("Rectangulos" = '0', "Trapecios" = '1')),
    plotOutput(ns("plot")),
    textOutput(ns('res'))
  )
}

intserver <- function(input, output, session)
{
  
  trapeval <- function(limits, f, eps)
  {
    x <- seq(from = limits[1], to = limits[2], by = eps)
    fi <- sapply(x, f)
    eps/2 * sum((fi[-length(x)] + fi[-1]))
  }
  
  recteval <- function(limits, f, eps)
  {
    x <- seq(from = limits[1], to = limits[2], by = eps)
    fi <- sapply(x, fx())
    eps * sum(fi)
  }
  
  fx <- reactive({
    f <- paste("aux <- function(x) ", input$fx)
    eval(parse(text = f))
    aux
  })
  
  integral <- reactive({
    switch (input$met,
      '0' = tmp <- recteval(c(input$min,input$max),fx(),input$eps),
      '1' = tmp <- trapeval(c(input$min,input$max),fx(),input$eps)
    )
    tmp
  })
  
  output$plot <- renderPlot({
    x <- seq(input$min, input$max, length.out = 100)
    y <- sapply(x, fx())
    plot(x,y, type = "l")
  })
  
  output$res <- renderText({
    integral()
  })
  
}