#Ley de los grandes numeros
#Ejemplo moneda

bignumui <- function(id)
{
  ns <- NS(id)
  tagList(
    h2("Ley de los grandes numeros"),
    selectInput(ns("inp"),choices = c("Ejemplo moneda" = "m", "Ejemplo cumpleanios" = "c"),label = "Ejemplo:", selected = "m"),
    plotOutput(ns("plt"))
  )
}

bignumumserv <- function(input, output, session)
{
  sim <- reactive({
    n <- 5000
    s <- 100
    means <- replicate(s, {
      tiros <- sample.int(2, size = n,replace = TRUE) - 1
      mean(tiros)
    })
    means
  })
  
  output$plt <- renderPlot({
    plot(sim(), ylim = c(0,1))
  })
}