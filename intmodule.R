library(ggplot2)
library(plyr)
require(ggplot2)

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
    ),
    h3('Multiple MonteCarlo'),
    #numericInput(ns('multi'), label = 'Numero de simulaciones', value = 100),
    plotOutput(ns('test'))
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
    return(data.frame('e' = e,'s' = s, 'U' = u, 'L' = l))
  }
  
  #Otra forma de correr mceval
  mc.intervals <- function(Phi, N, X.dens=runif, alpha=0.05){
    results.list <- lapply(N, function(nsim){
      X <- sapply(FUN=X.dens, nsim)
      PhiX <- sapply(X, Phi)
      estim <- mean(PhiX)
      quant <- qnorm(alpha/2, lower.tail=FALSE)
      S2 <- var(PhiX) # Estimate of the variance of phi(X_i)
      int.upper <- estim + sqrt(S2/nsim)*quant 
      int.lower <- estim - sqrt(S2/nsim)*quant 
      return(data.frame(N=nsim, Estimate=estim, LI=int.lower, UI=int.upper))
    })
    results.table <- ldply(results.list)
    return(results.table)
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
    plot(x,y, type = "l", main = paste('f(x)'))
  })
  
  output$test <- renderPlot ({
    N <- seq(from=1000, to=10000, by=1000)
    X.dens <- function(nsim) runif(nsim, 0, 2)
    data <- mc.intervals(Phi=fx(), N=N, X.dens=X.dens)
    ggplot(data, aes(x=N)) +
      geom_ribbon(aes(ymin=LI, ymax=UI), fill="grey", alpha=.4) + 
      geom_line(aes(y=Estimate), colour="blue")
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
    l <- integral()$U
    if(is.null(l))
      'Null'
    else
      l
  })
  
}