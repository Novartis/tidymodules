
ColSelector <- R6::R6Class(
  "ColSelector",
  inherit = Panel,
  public = list(
    initialize = function(...) {
      super$initialize(...)
      self$definePort({
        self$addInputPort(
          name = "data",
          description = "Any data table",
          sample = data.frame(a = 1:4, b = 1:4)
        )
        self$addOutputPort(
          name = "mapping",
          description = "dynamic list of column selections",
          sample = list(
            mapping1 = c("col1", "col2", "col3"),
            mapping2 = c("col2", "col4")
          )
        )
      })
    },
    ui = function() {
      super$ui(
        status = "primary",
        shiny::actionButton(self$ns("add"), label = "Add mapping", icon = icon("plus")),
        br(),
        tags$div(id = self$ns("uio_selector"))
      )
    },
    server = function(input, output, session) {
      # Mandatory
      super$server(input, output, session)

      self$react$cols <- reactiveValues(
        current = NULL,
        names = NULL,
        mapping = list()
      )

      self$obser$reset <- observe({
        dataPort <- self$getInput("data")
        req(dataPort)
        d <- dataPort()
        self$react$cols$mapping <- list()
        req(!is.null(d))
        self$react$cols$names <- colnames(d)
        self$react$cols$current <- 1
        shiny::removeUI(
          selector = paste0("#", self$ns("uio_selector div")),
          multiple = TRUE
        )
      })

      self$obser$add <- observeEvent(input$add, {
        d <- self$getInput("data")()
        req(!is.null(d))
        insertUI(
          selector = paste0("#", self$ns("uio_selector")),
          where = "beforeEnd",
          session = session,
          ui = tagList(
            selectizeInput(
              inputId = self$ns(paste0("mapping-", self$react$cols$current)),
              label = paste0("mapping-", self$react$cols$current),
              multiple = T,
              choices = self$react$cols$names,
              options = list(maxItems = 4L)
            )
          )
        )

        self$react$cols$mapping[[paste0("mapping-", self$react$cols$current)]] <- c("")
        self$react$cols$current <- self$react$cols$current + 1
      })

      self$obser$mappingKey <- observe({
        key <- paste0("mapping-", self$react$cols$current - 1)
        observeEvent(input[[key]], {
          self$react$cols$mapping[[key]] <- input[[key]]
        })
      })

      # Server logic
      # Ports assignment starts here
      self$assignPort({
        self$updateOutputPort(
          id = "mapping",
          output = reactive({
            self$react$cols$mapping
          })
        )
      })

      return(reactive(self$react$cols$mapping))
    }
  ),
  private = list(
    # any private functions?
  )
)
