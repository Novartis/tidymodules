
DatasetSelector <- R6::R6Class(
  classname = "DatasetSelector",
  inherit = Panel,
  public = list(
    initialize = function(...) {
      # Mandatory
      super$initialize(...)

      # Ports definition starts here
      self$definePort({
        # Add an output port (optional)
        self$addOutputPort(
          name = "dataset",
          description = "the selected dataset from environment by data()",
          sample = head(mtcars)
        )
      })
    },
    ui = function() {
      super$ui(
        status = "primary",
        fluidRow(
          column(12, uiOutput(outputId = self$ns("selectDatasetUI"))),
          column(12,
            style = "overflow:auto",
            tableOutput(
              outputId = self$ns("selectedDataset")
            )
          )
        )
      )
    },
    server = function(input, output, session) {
      # Mandatory
      super$server(input, output, session)

      output$selectDatasetUI <- renderUI({
        allDatasets <- as.data.frame(data()$results)
        allDatasets <- as.character(allDatasets$Item)
        selectInput(
          inputId = self$ns("selectData"),
          label = "Select dataset",
          choices = allDatasets,
          selected = allDatasets[1],
          selectize = TRUE
        )
      })
      output$selectedDataset <- renderTable({
        allDatasets <- as.data.frame(data()$results)
        dataset <- self$react$getData()
        if (is(dataset, "data.frame")) {
          allDatasets[which(allDatasets$Item == input$selectData), ]
        } else {
          "Not a table!"
        }
      })

      self$react$getData <- reactive({
        tryCatch(
          {
            dataName <- input$selectData
            req(!is.null(dataName))
            print(dataName)
            dataset <- get(dataName)
          },
          error = function(e) {
            NULL
          }
        )
      })

      # Server logic
      self$react$modOut <- reactive({
        dataset <- self$react$getData()
        req(is(dataset, "data.frame"))
        print(dim(dataset))
        return(dataset)
      })

      # Ports assignment starts here
      self$assignPort({
        self$updateOutputPort(
          id = "dataset",
          output = self$react$modOut
        )
      })

      return(self$react$modOut)
    }
  ),
  private = list(
    # any private functions?
  )
)
