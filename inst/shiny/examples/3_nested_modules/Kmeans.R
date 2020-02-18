
Kmeans <- R6::R6Class(
  "Kmeans", 
  inherit = tidymodules::TidyModule,
  public = list(
    # Example with nested module in list
    # nested_mods = list(CP = NULL),
    nested_mod = NULL,
    initialize = function(...){
      super$initialize(...)
      # self$nested_mods$CP <- ColorPicker$new("CP")
      self$nested_mod <- ColorPicker$new("CP")
      
      km_sample = kmeans(matrix(rnorm(100),ncol = 2),3)
      self$definePort({
        self$addOutputPort(
          name = "km",
          description = "object of class 'kmeans'",
          sample = km_sample)
      })
      
      
    },
    ui = function(label) {
      # col <- self$nested_mods$CP$ui("Color scheme")
      col <- self$nested_mod$ui("Color scheme")
      tags <- tagList(
        selectInput(self$ns('xcol'), 'X Variable', names(iris)),
        selectInput(self$ns('ycol'), 'Y Variable', names(iris), selected=names(iris)[[2]]),
        numericInput(self$ns('clusters'), 'Cluster count', 3, min = 1, max = 9))
      
      fluidRow(
        column(4, tags, col),
        column(8, plotOutput(self$ns("plot1")))
      )
    },
    server = function(input, output, session){
      
      super$server(input,output,session)
      
      # self$nested_mods$CP$callModule()
      self$nested_mod$callModule()
      
      # Combine the selected variables into a new data frame
      selectedData <- reactive({
        iris[, c(input$xcol, input$ycol)]
      })
      
      clusters <- reactive({
        kmeans(selectedData(), input$clusters)
      })
      
      output$plot1 <- renderPlot({
        # Get ColorPicker output, default to first output port
        # cp <- self$nested_mods$CP$getOutput()
        cp <- self$nested_mod$execOutput()
        cols <- brewer.pal(input$clusters, cp$scheme)
        cols <- adjustcolor(cols, alpha.f = cp$transparency)
        if(cp$reverse)
          cols <- rev(cols)
        cols <- cols[clusters()$cluster]
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = cols,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
      })
      
      self$assignPort({
        self$updateOutputPort(
          id = "km",
          output = clusters)
      })
      
      return(clusters)
    }
  )
)

