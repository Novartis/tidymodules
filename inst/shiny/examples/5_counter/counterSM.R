# Example from Joe' blog (slightly modified)
# https://shiny.rstudio.com/articles/modules.html

library(shiny)

counterButton <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label),
    verbatimTextOutput(ns("out"))
  )
}

counter <- function(input, output, session, reset) {
  count <- reactiveVal(0)
  observeEvent(input$button, {
    count(count() + 1)
  })
  observeEvent(reset(),{
    count(0)
  })
  output$out <- renderText({
    count()
  })
  count
}
