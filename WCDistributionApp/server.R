## VG TO DO: 
# Add special points to teh grid so you always see xmas tree points and deltas and end poitns


source("boundingFunctions.R")
library(shiny)

shinyServer(function(input, output) {
  # Reactive version of user inputs.. does validaton.  
  params <- reactive({
    validate(
      need(input$c < input$mu && input$mu < input$vmax && input$c >= 0,
           "Please specify 0 <= vmin <= mu <= vmax.")
    )
    make_unitless(input$c, input$mu, input$vmax)  
  })

  ####  
  #The actual UI elements  
  ####
  output$boundPlot <- renderPlot({
      wc_plot(params()$S, params()$M, params()$mu)
  })

  output$ccdfPlot <- renderPlot({
    req(params())
    validate(need(input$BPair, 
                  message = "\n\n\n\n\nSelect a point on graph to see best-case valuation distribution."),
                    errorClass = "selectPoint") 
    B <- input$BPair$x
    D <- min(B/2/input$mu, delta_H(params()$S, params()$M))
    D <- max(D, delta_L(params()$S, params()$M))
    ccdf_plot(params()$S, params()$M, D, input$mu)
  })
})
