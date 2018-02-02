library(shiny)
library(shinycssloaders)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("GuessWord - The App that guesses your next ..."),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Application", 
                         br(), br(),
                         h3("Enter A Phrase:"),
                         textInput("phrase", "", value = "", width=500),
                         br(), br(),
                         h3("Most Likely Next Words:"),
                         withSpinner(dataTableOutput("guess", width = "300px"))),
                tabPanel("Instructions", includeHTML("instructions.html")) 

    )
  )
))
