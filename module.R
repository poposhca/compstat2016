library(shiny)

modui <- function(id)
{
  ns <- NS(id)
  tagList(
    p("Module"),
    numericInput(ns("num"), "Numero:", value = 3),
    p("Hola :)"),
    textOutput(ns("out"))
  )
}

modserv <- function(input, output, session)
{
  output$out <- renderText({
    return(input$num)
  })
}