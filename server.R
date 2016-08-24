shinyServer(function(input, output){
  
  Finv <- function(u, lambda){
    return (-log(1-u)/lambda)
  }
  
  GetV <- function(n){
    return (runif(n))
  }
  
  output$invgraph <- renderPlot({
    v <- GetV(input$n)
    f <- Finv(v,input$lmd)
    return (hist(f))
  })
  
})