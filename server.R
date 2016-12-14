library(shiny)

shinyServer(function(input, output, session){
  
  v <- reactiveValues(page = "none")
  
  m1 <- callModule(fiserv, "m1")
  m2 <- callModule(acreserver,"m2")
  m3 <- callModule(bignumumserv,"m3")
  m4 <- callModule(intserver, "m4")
  m5 <- callModule(mcserver, "m5")
  m6 <- callModule(mhserver, "m6")
  
  #Eventos que escuchan a los links
  observeEvent(input$finv,{
    v$page = "inv"
  })
  observeEvent(input$acre,{
    v$page = "acre"
  })
  observeEvent(input$big,{
    v$page = "big"
  })
  observeEvent(input$integral,{
    v$page = "integral"
  })
  observeEvent(input$mc,{
    v$page = "mc"
  })
  observeEvent(input$mh,{
    v$page = "mh"
  })
  
  output$ui <- renderUI(
    switch (v$page,
      "none" = list(p("Selecciona una opcion de la barra lateral")),
      "inv" = finvdui("m1"),
      "acre" = acreui("m2"),
      "big" = bignumui("m3"),
      "integral" = intui("m4"),
      "mc" = mcui("m5"),
      "mh" = mhui("m6")
    )
  )

})