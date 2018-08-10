
library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Next Word Prediction"),
  
  fluidRow(
      column(12,
             br(),
             h4("This application tries to predict the next word in a phrase."),
             br(),
             h4("To run the application, type a phrase in the below box."),
             br(),
             h4("besides the input box you will find the predicted word"),
             br()
      )
  ),
  
  # Enter the text here
  sidebarLayout(
    sidebarPanel(
       textInput("InputSentence", 
                 label = "Enter your text here", 
                 value = " ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        h3("Predicted Word"),
        textOutput("text1"),
        tags$head(tags$style("#text1{color: red;
        font-style: italic;}"
                         )
        )
    )
  )
))
