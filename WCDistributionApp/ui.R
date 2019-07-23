# This is the user-interface definition of a Shiny web application.
library(shiny)
library(shinyBS)

shinyUI(fluidPage(

  ##VG Update this to a smarter set formatting for initial instructions.
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-selectPoint {
                    text-align: center
                    }
                    "))
    ),

    #Actual layout below
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
    column(3, 
           br()
    ),
      column(2, 
           numericInput("c", label = "Min Valuation", value = 0.),
           bsTooltip("c", "What is the least amount a customer might pay for product?", placement = "bottom",
                     options = NULL)
      ),
    column(2, 
           numericInput("mu", label = "Avg. Valuation", value = 1),
           bsTooltip("mu", "What would a customer pay for product?", placement = "bottom",
                     options = NULL)
    ),
    column(2, 
           numericInput("vmax", label = "Max Valuation", value = 4), 
           bsTooltip("vmax", "What is the maximum customer might pay for product?", placement = "bottom",
                     options = NULL)
    ) 
  ),
  fluidRow(
    column(6,
      plotOutput("boundPlot", click="BPair"),
      bsTooltip("boundPlot", "Click any point to show valuation distribution", placement = "right",
                options = NULL)
    ),
    
    column(6, 
      plotOutput("ccdfPlot")
    )
  )
))
