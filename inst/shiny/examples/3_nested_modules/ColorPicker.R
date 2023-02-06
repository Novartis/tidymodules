
ColorPicker <- R6::R6Class(
  "ColorPicker",
  inherit = Panel,
  public = list(
    initialize = function(...) {
      super$initialize(...)

      self$definePort({
        self$addOutputPort(
          name = "scheme",
          description = "string defining color scheme",
          sample = list(scheme = "Dark2", reverse = FALSE, transparency = 1)
        )
      })
    },
    ui = function(label = "Coloring") {
      super$ui(
        status = "primary",
        tagList(
          selectInput(self$ns("scheme"), label = label, choices = c("Dark2" = "Dark2", "Set1" = "Set1", "Set2" = "Set2"), selected = "Dark2"),
          checkboxInput(self$ns("reverse"), label = "Reverse scheme"),
          sliderInput(self$ns("transparency"), label = "Transparency", min = 0, max = 1, value = 1)
        )
      )
    },
    server = function(input, output, session) {
      super$server(input, output, session)

      self$obser$log <- observe({
        msg <- paste("Color scheme was selected", input$scheme)
        cat(msg, "\n")
      })

      self$react$input_values <- reactive({
        reactiveValuesToList(input)
      })

      self$assignPort({
        self$updateOutputPort(
          id = "scheme",
          output = self$react$input_values
        )
      })

      return(input)
    }
  )
)
