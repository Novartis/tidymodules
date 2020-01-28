library(tidymodules)

source(system.file(package = "tidymodules","shiny/examples/1_simple_addition/Addition.R"))

Addition$new()
Addition$new()

ui <- fixedPage(
  h2("tidymodules : Simple Addition"),
  shiny::fluidRow(
    sliderInput("first_number",label = "Enter your first number",min = 1,max = 100,value = 1),br(),
    mod(1)$ui(),br(),
    mod(2)$ui(),br(),
    "Total: ",textOutput("total_result")
  )
)

server <- function(input, output, session) {
  callModules()
  
  first<-reactive({
    req(input$first_number)
  })
  
  observe({
    first %>1% mod(1) %1>1% mod(2)
  })
  
  output$total_result <- renderText({
    result <- mod(2)$getOutput(1)
    result()
  })
}

shinyApp(ui, server)