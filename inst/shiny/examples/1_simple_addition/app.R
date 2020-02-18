library(shiny)

# SM -----------> Shiny module

source(system.file(package = "tidymodules","shiny/examples/1_simple_addition/AdditionSM.R"))

add_html_1 <- AdditionSM_UI("Addition_1")
add_html_2 <- AdditionSM_UI("Addition_2")

SM_UI <- shiny::basicPage(
  h2("Shiny module : Simple Addition"),
    sliderInput("first_number",label = "Enter your first number",min = 1,max = 100,value = 1),br(),
    add_html_1, br(),
    add_html_2, br(),
    "Total: ",textOutput("total_result")
)

SM_Server <- function(input, output, session) {
  first<-reactive({
    req(input$first_number)
  })
  
  second <- callModule(module = AdditionTM_Server, id = "Addition_1", first)
  result <- callModule(module = AdditionTM_Server, id = "Addition_2", second)
  
  output$total_result <- renderText({
    result()
  })
}

# TM -----------> tidymodules

library(tidymodules)

source(system.file(package = "tidymodules","shiny/examples/1_simple_addition/Addition.R"))


Addition$new()
Addition$new()

TM_UI <- shiny::basicPage(
  h2("tidymodules : Simple Addition"),
  sliderInput("first_number",label = "Enter your first number",min = 1,max = 100,value = 1),br(),
  mod(1)$ui(),br(),
  mod(2)$ui(),br(),
  "Total: ",textOutput("total_result")
)

TM_Server <- function(input, output, session) {
  callModules()
  
  first<-reactive({
    req(input$first_number)
  })
  
  observe({
    first %>1% mod(1) %1>1% mod(2)
  })
  
  output$total_result <- renderText({
    mod(2)$execOutput(1)
  })
}

### Start the app

# shinyApp(SM_UI, SM_Server)
shinyApp(TM_UI, TM_Server)
