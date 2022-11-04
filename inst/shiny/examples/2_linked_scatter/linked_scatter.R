
input_sample <- head(mpg)

LinkedScatter <- R6::R6Class(
  "LinkedScatter",
  inherit = tidymodules::TidyModule,
  public = list(
    initialize = function(...) {
      super$initialize(...)

      self$definePort({
        self$addInputPort(
          name = "data",
          description = "Any rectangular data frame",
          sample = input_sample
        )

        self$addInputPort(
          name = "option",
          description = "The module options are two vectors of (x,y) column names defining the left and right plot",
          sample = list(
            left = c("cty", "hwy"),
            right = c("drv", "hwy")
          )
        )

        self$addOutputPort(
          name = "selection",
          description = "The input data frame with a new column for selected rows",
          sample = input_sample
        )


        self$addOutputPort(
          name = "selection_only",
          description = "Only the selected rows from ggplot brushing",
          sample = input_sample
        )
      })
    },
    ui = function() {
      fluidRow(
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput(self$ns("plot1"), brush = self$ns("brush")),
            type = 3, color.background = "white"
          )
        ),
        column(
          6,
          shinycssloaders::withSpinner(
            plotOutput(self$ns("plot2"), brush = self$ns("brush")),
            type = 3, color.background = "white"
          )
        )
      )
    },
    server = function(input, output, session, ...) {
      super$server(input, output, session, ...)

      args <- list(...)

      self$assignPort({
        self$updateInputPort(
          id = "data",
          input = args$data
        )

        self$updateInputPort(
          id = "option",
          input = args$options
        )
      })

      self$react$dataWithSelection <- reactive({
        data <- self$execInput("data")
        req(nrow(data) != 0)
        brushedPoints(data, input$brush, allRows = TRUE)
      })

      self$react$dataWithSelectionOnly <- reactive({
        data <- self$execInput("data")
        req(nrow(data) != 0)
        brushedPoints(data, input$brush, allRows = FALSE)
      })

      output$plot1 <- renderPlot({
        o <- self$execInput("option")
        req(o)
        private$scatterPlot(self$react$dataWithSelection(), o$left)
      })

      output$plot2 <- renderPlot({
        o <- self$execInput("option")
        req(o)
        private$scatterPlot(self$react$dataWithSelection(), o$right)
      })

      self$assignPort({
        self$updateOutputPort(
          id = "selection",
          output = self$react$dataWithSelection
        )

        self$updateOutputPort(
          id = "selection_only",
          output = self$react$dataWithSelectionOnly
        )
      })

      return(self$react$dataWithSelection)
    }
  ),
  private = list(
    scatterPlot = function(data, cols) {
      ggplot(data, aes_string(x = cols[1], y = cols[2])) +
        geom_point(aes(color = selected_)) +
        scale_color_manual(values = c("black", "#66D65C"), guide = "none")
    }
  )
)
