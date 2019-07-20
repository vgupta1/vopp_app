# This is the user-interface definition of a Shiny web application.
library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(12,
           includeMarkdown("blurb.md")
    )
  ),
  fluidRow(
    column(12,
           br()
    )
  ),
  fluidRow(
    column(2, 
           numericInput("c", label = "Min Valuation", value = 0.)
    ),
    column(2, 
           numericInput("mu", label = "Avg Valuation", value = 1)
    ),
    column(2, 
           numericInput("vmax", label = "Max Valuation", value = 4)
    ), 
    column(6, 
           uiOutput("outBSlider")
    )
  ),
  fluidRow(
    column(6,
      plotOutput("boundPlot")
    ),
    
    column(6, 
      plotOutput("ccdfPlot")
    )
  )
))
