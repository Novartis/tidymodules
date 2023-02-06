
DataFilter <- R6::R6Class(
  "DataFilter",
  inherit = Panel,
  public = list(
    initialize = function(...) {
      # Mandatory
      super$initialize(...)

      # Ports definition starts here
      self$definePort({
        # At least one input
        self$addInputPort(
          name = "data",
          description = "Any rectangular data frame",
          sample = head(cars)
        )

        self$addOutputPort(
          name = "selected",
          description = "The original data frame with a new boolean column `selected` that indicates whether the new is selected.",
          sample = data.frame(a = c(1, 2), selected = c(TRUE, FALSE))
        )

        self$addOutputPort(
          name = "filtered",
          description = "The data frame containing only the selected rows.",
          sample = mtcars[c(1, 2, 5), ]
        )
      })
    },
    ui = function() {
      super$ui(
        status = "primary",
        tags$div(
          style = "overflow:auto",
          DT::dataTableOutput(self$ns("dtOutput"))
        )
      )
    },
    server = function(input, output, session) {
      # Mandatory
      super$server(input, output, session)

      # Server logic
      self$react$modOut <- reactive({
        d <- self$getInput("data")
        req(!is.null(d))
      })

      self$react$dtReturn <- reactive({
        d <- self$getInput("data")
        req(d)
        DT::datatable(d())
      })

      DTproxy <- DT::dataTableProxy("dtOutput", session = session)
      self$obser$clearRows <- observeEvent(input$clearRows, {
        DT::selectRows(DTproxy, NULL)
      })

      # Get a boolean vector indicating whether a row is selected
      self$react$selectedRowsBoolean <- reactive({
        d <- self$getInput("data")
        req(input$dtOutput_rows_selected, d)

        # Get all the row indices
        selected <- seq_len(nrow(d())) %in% input$dtOutput_rows_selected

        selected
      })

      # The data frame with only selected rows
      self$react$selectedRows <- reactive({
        d <- self$getInput("data")
        d()[self$react$selectedRowsBoolean(), ]
      })

      # The data frame with a `selected` boolean column
      self$react$dataSelected <- reactive({
        d <- self$getInput("data")

        cbind(d(), selected = self$react$selectedRowsBoolean())
      })

      output$dtOutput <- DT::renderDataTable({
        self$react$dtReturn()
      })

      # Ports assignment starts here
      self$assignPort({
        self$updateOutputPort(
          id = "selected",
          output = self$react$dataSelected
        )

        self$updateOutputPort(
          id = "filtered",
          output = self$react$selectedRows
        )
      })
    }
  ),
  private = list(
    # any private functions?
  )
)
