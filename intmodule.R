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
    numericInput(ns('alpha'), label = 'Nivel de confianza (%):', value = '95', min = 0, max = 99, step = 1),
    selectInput(inputId = ns("met"), "Metodo de integracion: ", 
                choices = c("Monte Carlo" = '0',"Rectangulos" = '1', "Trapecios" = '2')),
    plotOutput(ns("plot")),
    fluidRow(
      column(4, span('Resultado de la integral:', style = 'color:#f95e5e')),
      column(3, textOutput(ns('res')))
    ),
    br(),
    p('Intervalos de confianza:', style = 'color:#f95e5e'),
    fluidRow(
      column(4, span('Limite inferior:')),
      column(3, textOutput(ns('L')))
    ),
    fluidRow(
      column(4, span('Limite superior:')),
      column(3, textOutput(ns('U')))
    )
  )
}

intserver <- function(input, output, session)
{
  
  mceval <- function(limits, f, eps)
  {
    n <- (limits[2]-limits[1])/eps
    u <- runif(n, min = limits[1], max = limits[2])
    h <- sapply(u, f)
    e <- mean(h) * (limits[2]-limits[1])
    s <- var(h)
    #Intervalos de confianza
    alpha = 1 - (input$alpha/100)
    z <- qnorm(alpha/2, lower.tail = FALSE)      #z alpha/2 derecho
    l <- e - sqrt(s/n) * z
    u <- e + sqrt(s/n) * z
    return(list('e' = e,'s' = s, 'U' = u, 'L' = l))
  }
  
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
      '0' = tmp <- mceval(c(input$min,input$max),fx(),input$eps),
      '1' = tmp <- list('e' = recteval(c(input$min,input$max),fx(),input$eps)),
      '2' = tmp <- list('e' = trapeval(c(input$min,input$max),fx(),input$eps))
    )
  })
  
  output$plot <- renderPlot({
    x <- seq(input$min, input$max, length.out = 100)
    y <- sapply(x, fx())
    plot(x,y, type = "l")
  })
  
  output$res <- renderText({
    integral()$e
  })
  
  output$L <- renderText({
    u <- integral()$L
    if(is.null(u))
      'Null'
    else
      u
  })
  
  output$U <- renderText({
    l <- integral()$L
    if(is.null(l))
      'Null'
    else
      l
  })
  
}