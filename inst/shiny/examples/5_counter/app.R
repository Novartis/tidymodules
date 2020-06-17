
# SM -----------> Shiny module

library(shiny)

source(system.file(package = "tidymodules","shiny/examples/5_counter/counterSM.R"))

SM_UI <- fluidPage(
  br(),
  shiny::fluidRow(
    column(1,actionButton("reset", label = "Reset")),
    column(1,counterButton("counter", "Counter")),
    column(1,"counter + 2 = ",textOutput("total_result"))
  )
)

SM_Server <- function(input, output, session) {
  reset <- reactive(input$reset)
  count <- callModule(counter, "counter", reset)
  output$total_result <- renderText({
    count() + 2
  })
}

# TM -----------> tidymodules

library(tidymodules)

source(system.file(package = "tidymodules","shiny/examples/5_counter/Counter.R"))

cnt <- Counter$new()

TM_UI <- fluidPage(
  br(),
  shiny::fluidRow(
    column(1,actionButton("reset", label = "Reset")),
    column(1,cnt$ui("Counter")),
    column(1,"counter + 2 = ",textOutput("total_result"))
  )
)

TM_Server <- function(input, output, session) {
  reset <- reactive(input$reset)
  
  callModules()
  defineEdges({
    reset %>1% cnt
  })
  
  output$total_result <- renderText({
    cnt$execOutput("counter") + 2
  })
}

# shinyApp(SM_UI, SM_Server)
shinyApp(TM_UI, TM_Server)