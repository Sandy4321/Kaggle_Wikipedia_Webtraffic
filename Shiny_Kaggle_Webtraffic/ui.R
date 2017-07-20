#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  # Application title
  titlePanel("Visualisation of Forecast Results"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
     
      selectInput("model", "Auswahl Model:",
                  c(models), 
                  multiple = TRUE,
                  selected = c("y", "p_ensemble" )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput('plot')
    )
  )
))
