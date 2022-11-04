#' UI function of the Shiny Addition module
#'
#' @description
#' Generated the UI elements of the Addition module.
#'
AdditionSM_UI <- function(ns_id) {
  ns <- NS(ns_id)

  div(
    style = paste0(
      "width:30%;",
      "background:lightgrey;",
      "border: solid;",
      "border-color: grey;",
      "padding: 20px;"),
    "Module input : ",
    textOutput(ns("left")),
    " + ",
    sliderInput(
      ns("right"),
      label = "Number to add",
      min = 1,
      max = 100,
      value = 1),
    " = ",
    textOutput(ns("total"))
  )
}

#' Server function of the Shiny Addition module
#'
#' @description
#' Server logic of the Shiny Addition module. 
#' Add an input number to a number selected by the user with a slider.
#'
AdditionSM_Server <- function(input, output, session, number) {
  sum_numbers <- reactive({
    req(input$right)
    req(number)
    as.numeric(number()) + as.numeric(input$right)
  })

  output$left <- renderText({
    req(number)
    number()
  })

  output$total <- renderText({
    sum_numbers()
  })

  return(sum_numbers)
}
