#'
#' Counter Module.
#'
#' @description
#' This \href{https://opensource.nibr.com/tidymodules}{`{tm}`} module is a R6 class representing a Counter.
#'
#' @family tm
#'
#' @details
#' More details about your module here.
#'
#'
#' @import tidymodules
#' @noRd
Counter <- R6::R6Class(
  classname = "Counter",
  inherit = TidyModule,
  public = list(
    #' @description
    #' Module's initialization function.
    #' @param ... options
    #' @return An instance of Counter
    initialize = function(...) {
      # Don't remove the line below
      super$initialize(...)

      # Ports definition starts here...
      self$definePort({
        self$addInputPort(
          name = "reset",
          description = "An integer of class 'shinyActionButtonValue'",
          sample = 1
        )

        self$addOutputPort(
          name = "counter",
          description = "An integer representing the current counter value",
          sample = 3
        )
      })
    },
    #' @description
    #' Module's ui function.
    #' @return HTML tags list.
    ui = function(label = "Counter") {
      tagList(
        actionButton(self$ns("button"), label = label),
        verbatimTextOutput(self$ns("out"))
      )
    },
    #' @description
    #' Module's server function.
    #' @param input Shiny input
    #' @param output Shiny output
    #' @param session Shiny session
    server = function(input, output, session) {
      # Don't remove the line below
      super$server(input, output, session)

      # Module server logic starts here ...
      self$react$count <- reactiveVal(0)
      self$obser$add <- observeEvent(input$button, {
        self$react$count(self$react$count() + 1)
      })
      self$obser$reset <- observeEvent(self$execInput("reset"), {
        self$react$count(0)
      })
      output$out <- renderText({
        self$react$count()
      })

      self$assignPort({
        self$updateOutputPort("counter", self$react$count)
      })
    }
  )
)
