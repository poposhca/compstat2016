shinyServer(function(input, output){
  
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
  
  #funcion que imprime en output test
  output$test <- renderUI(
    sayhi()
  )
  
  #Evento que escucha al hipervinculo
  sayhi <- eventReactive(input$link,{
    list(
      h1("Proxima tarea", style="color:red"),
      textInput("text2", "text2",label = "texto")
    )
  })
  
})