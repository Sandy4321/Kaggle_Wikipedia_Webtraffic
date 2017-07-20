#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  results_plot <- reactive({
    results_df  %>%
      filter(model %in% input$model)
  })
  
  output$plot <- renderPlot({
    ggplot(results_plot(), aes(x = Date, y = value, color = model)) +
      geom_line()})
  
}
)
