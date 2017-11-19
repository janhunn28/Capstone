#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

## SHINY UI
shinyUI(
  fluidPage(
    titlePanel("Text Prediction - Capstone Project - JHU Data Science Certification"),
    sidebarLayout(
      sidebarPanel(
        helpText("Please enter a word or phrase and the next word will be predicted."),
        hr(),
        textInput("in_string", "In the box below, input the word or phrase",value = ""),
        hr(),
        helpText("The most common word will be shown on the right as the predicted word."), 
        hr()
      ),
      mainPanel(
        strong("Entered word or Phrase is:"),
        strong(code(textOutput('sentence1'))),
        br(),
        strong("Predicted word is:"),
        strong(code(textOutput('sentence2'))),
        hr()
        
      )
    )
  )
)

