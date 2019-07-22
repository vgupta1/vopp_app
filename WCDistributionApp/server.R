##The actual workhorse...
# Rescale worst-case distributions to correct scale.  Account for $c$
# Add an indicator of what regime we are in somewhere?
# Check the math here about what you're actually graphing?

source("boundingFunctions.R")

library(shiny)

shinyServer(function(input, output) {
  
  # Reactive objects build from user inputs
  params <- reactive({
    validate(
      need(input$c < input$mu && input$mu < input$vmax && input$c >= 0,
           "Please specify 0 < vmin < mu < vmax.")
    )
    make_unitless(input$c, input$mu, input$vmax)  
  })
  
  #VG Change this to non-reactive?
  max_B_rounded <- reactive({
    floor(100 * delta_H(params()$S, params()$M) * 2 * input$mu) / 100
  })

  #The actual UI elements  
  output$boundPlot <- renderPlot({
      D <- input$B/2/input$mu

      #Change this so you're not invoking twice?
      #Does params even need to be reactive?
      wc_plot(params()$S, params()$M, params()$mu) + 
        geom_vline(xintercept = input$B,
                 color="blue") +
        geom_point(x=input$B, y=wc_bound(params()$S, params()$M, D) - 1, 
                   fill="red", size=5, color="red")
  })

  output$ccdfPlot <- renderPlot({
    D <- input$B/2/input$mu
    ccdf_plot(params()$S, params()$M, D, input$mu)
      })

  output$outBSlider <- renderUI({
    ##Do some light validation on values.  
    
    
    sliderInput("B", "Market Hetereogeneity / Mean Abs. Deviation", 
                min = 0,
                max = max_B_rounded(), 
                val = ifelse(input$B > max_B_rounded(), 
                             max_B_rounded(), 
                             input$B), 
                step = max_B_rounded()/20
                )
  })
})






