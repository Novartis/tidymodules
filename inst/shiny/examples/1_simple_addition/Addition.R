#'
#' Addition module implemented with {tidymodules}
#'
#' @description
#' Take a number as input and add it to the user selection.
#'
#' @details
#' Should be initialized and injected in your application.
#' Input port:
#'  - left
#' Output port:
#'  - total
#'
#' @example
#'
#' a <- Addition$new()
#'
#'
Addition <- R6::R6Class(
  "Addition",
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...) {
      # mandatory
      super$initialize(...)

      self$definePort({
        self$addInputPort(
          name = "left",
          description = "input value to add to the user selected number",
          sample = 5
        )

        self$addOutputPort(
          name = "total",
          description = "Sum of the two numbers",
          sample = 6
        )
      })
    },
    #' @description
    #' Store's ui function.
    #' @return UI elements.
    ui = function() {
      div(
        style = paste0(
          "width:30%;",
          "background:lightgrey;",
          "border: solid;",
          "border-color: grey;",
          "padding: 20px;"),
        "Module input : ",
        textOutput(self$ns("left")),
        " + ",
        sliderInput(
          self$ns("right"),
          label = "Number to add",
          min = 1,
          max = 100,
          value = 1),
        " = ",
        textOutput(self$ns("total"))
      )
    },
    #' @description
    #' Store's server function.
    #' @param input Shiny input.
    #' @param output Shiny output
    #' @param session Shiny session
    server = function(input, output, session) {
      # Mandatory
      super$server(input, output, session)

      self$react$sum_numbers <- reactive({
        req(input$right)
        left <- self$execInput(1)
        as.numeric(left) + as.numeric(input$right)
      })

      output$left <- renderText({
        self$execInput(1)
      })

      output$total <- renderText({
        self$react$sum_numbers()
      })

      self$assignPort({
        self$updateOutputPort(
          id = "total",
          output = self$react$sum_numbers
        )
      })
    }
  )
)
