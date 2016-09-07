source("inv.R")
source("acre.R")

shinyServer(function(input, output){
  
  v <- reactiveValues(page = "none")
  
  Finv <- function(u, lambda){
    return (-log(1-u)/lambda)
  }
  
  GetV <- function(n){
    return (runif(n))
  }
  
  bondad <- function(u){
    chisq.test(u)
  }
  
  output$invgraph <- renderPlot({
    v <- GetV(input$n)
    f <- Finv(v,input$lmd)
    hist(f)
  })
  
  output$chi <- renderText({
    v <- GetV(input$n)
    f <- Finv(v,input$lmd)
    bondad(f)$p.value
  })
  
  #Aceptación Rechazo
  
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
  
  #Eventos que escuchan a los links
  observeEvent(input$finv,{
    v$page = "inv"
  })
  observeEvent(input$acre,{
    v$page = "acre"
  })
  
  #funcion que imprime en output test
  output$ui <- renderUI(
    switch (v$page,
      "none" = list(p("Selecciona una opción de la barra lateral")),
      "inv" = invui(),
      "acre" = acreui()
    )
  )
  
})