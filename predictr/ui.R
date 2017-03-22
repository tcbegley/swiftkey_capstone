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
  titlePanel("Next word predictor"),
  
  textInput("usertext", "Enter text here:", width="100%"),
  
  hr(),
  
  fluidRow(column(12,
      column(4,
             h5("Prediction 1:"),
             textOutput("pred1")
      ),
      column(4,
             h5("Prediction 2:"),
             textOutput("pred2")
      ),
      column(4,
             h5("Prediction 3:"),
             textOutput("pred3")
      )), align="center"),
  
  hr(),
  
  h4("About this app"),
  
  p("This app takes text input supplied by you and aims to predict the most likely next word.
    Three predictions are made in decreasing order of likelihood. This app forms part of my 
    submission to the capstone project from the Coursera Data Science Specialisation"),
  p("The algorithm used is the 'Stupid Backoff' method which can be read about at the below link.
    The algorithm was trained on data provided by swiftkey scraped from twitter, blogs and news 
    sources. Code used to clean the data and prepare the model can be viewed at the linked github 
    repository."),
  a(href="http://www.aclweb.org/anthology/D07-1090.pdf", "Link to description of algorithm."),
  br(),
  a(href="https://github.com/tcbegley/swiftkey_capstone", "Link to code on github.")
  
))
