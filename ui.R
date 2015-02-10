
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("nextPlot", "Another Relationship!")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("scatterPlot"),
      dataTableOutput("summaryTable"),
      plotOutput("linePlot")
    )
  )
))
