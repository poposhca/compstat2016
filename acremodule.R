library(shiny)

acreui <- function(id)
{
  ns <- NS(id)
  tagList(
    h1("Simulación Aceptación Rechazo"),
    fluidRow(
      column(6, numericInput(ns("xmin"), label = "X min: ", value = 0)),
      column(6, numericInput(ns("xmax"), label = "X max: ", value = 10))
    ),
    p("Funciones g(x) y f(x)"),
    fluidRow(
      column(6, textInput(ns("fx"),"f(x):", value = "sin(x)")),
      column(6, selectInput(ns("gx"), label = "g(x):",
           choices=c("Distribucion uniforme"="unif",
          "Distribucion normal"="norm",
          "Distribucion exponencial"="exp"),
        selected = "unif"))
    ),
    fluidRow(
      column(6, sliderInput(ns("m"),"M:", min = 1, max = 10, step = 0.5, value = 1)),
      column(6, numericInput(ns("nsim"), label = "Numero de simulaciones: ", value = 100))
    ),
    plotOutput(ns("plt")),
    plotOutput(ns("sim"))
  )
}

acreserver <- function(input, output, session)
{
  #El mundo
  gx <- reactive({
    switch (input$gx,
      "unif" = function(x) 1*(x>0 && x<1),
      "norm" = function(x) dnorm(x),
      "exp" = function(x) dexp(x)*(x>0)
    )
    
  })
  
  #Lo que queremos
  fx <- reactive({
    f <- paste("aux <- function(x) ", input$fx)
    eval(parse(text = f))
    aux
  })
  
  dosim <- reactive({
    n <- 0
    a <- numeric(input$nsim) #Vector de flotantes
    g <- switch (input$gx,
      "unif" = function() runif(1),
      "norm" = function() rnorm(1),
      "exp" = function() rexp(1)
    )
    while(n < input$nsim)
    {
      y <- g()
      u <- runif(1)
      if(y >= input$xmin && y<=input$xmax && u <= fx()(y)/input$m*gx()(y))
      {
        a[n] <- y
        n <- n + 1
      }
    }
    a
  })
  
  output$plt <- renderPlot({
    x <- seq(input$xmin, input$xmax, length.out=100)
    y1 = input$m*sapply(x, gx())
    y2 = sapply(x, fx())
    plot_limit = c(min(c(y1, y2)), max(c(y1, y2)))
    plot(x,y1, ylim = plot_limit, main = "G(x) y F(x)")
    lines(x,y2)
  })

  output$sim <- renderPlot({
    hist(dosim(), main = "Simulaciones")
  })
  
}