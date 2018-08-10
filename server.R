
library(shiny)
source("./WordPrediction.R")

# Define server logic required to output predcited word
shinyServer(function(input, output) {
   
  output$text1 <- renderText({
    
      nextWordPred(input$InputSentence)
    
  })
  
})
