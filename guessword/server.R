library(shiny)
library(tidyverse)
library(tidytext)
library(data.table)
library(DT)

options(warn =-1)

## load perdiction functions and probability data
source("predict.R")

shinyServer(function(input, output) {
  
  ## text input empty at launch so no spaces
  last_space <- reactiveValues(value = -1)
  
  ## generate an event to update the location of last space if it changed
  observeEvent(input$phrase, {
    space_pos <- ifelse(nchar(input$phrase) == 0, 0,
                              sapply(gregexpr(" ", input$phrase), tail, 1))
    if (space_pos != last_space$value) last_space$value <- space_pos
    })

  ## Process phrase for next word if a new word has been added
  bestguess <- eventReactive(last_space$value, {
    if(nchar(input$phrase) == 0) last_space$value <- -1
    process_phrase(input$phrase)
    })
  
  ## Output the five guesses based on model as a table
  output$guess <- renderDataTable({
      datatable(bestguess(), 
                options = list(dom = 't',
                               columnDefs = list(list(width = '100px', targets = c(0,1,2)))), 
                colnames = c("Word", "Probability")) %>%
      formatPercentage("p_word", 4)
    })
  
})
