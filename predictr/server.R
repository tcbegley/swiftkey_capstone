#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("predict2.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    pred <- reactive({next_word(input$usertext)})
   
    output$pred1 <- renderText({as.character(pred()[1,1])})
    output$pred2 <- renderText({as.character(pred()[2,1])})
    output$pred3 <- renderText({as.character(pred()[3,1])})
})
