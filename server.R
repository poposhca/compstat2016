library(shiny)

shinyServer(function(input, output, session){
  
  v <- reactiveValues(page = "none")
  
  m1 <- callModule(fiserv, "m1")
  m2 <- callModule(acreserver,"m2")
  
  #Eventos que escuchan a los links
  observeEvent(input$finv,{
    v$page = "inv"
  })
  observeEvent(input$acre,{
    v$page = "acre"
  })
  
  output$ui <- renderUI(
    switch (v$page,
      "none" = list(p("Selecciona una opción de la barra lateral")),
      "inv" = finvdui("m1"),
      "acre" = acreui("m2")
    )
  )
  
})