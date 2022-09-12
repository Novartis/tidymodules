
BasePlot <- R6::R6Class(
  "BasePlot", 
  inherit = ClosablePanel,
  public = list(
    initialize = function(...){
      # Mandatory
      super$initialize(...)
      
      # Ports definition starts here
      self$definePort({
        # At least one input
        self$addInputPort(
          name = "data",
          description = "Any rectangular data frame",
          sample = head(mtcars))
        
        self$addInputPort(
          name = "mapping",
          description = "vector of column names for the mapping",
          sample =  colnames(mtcars)[1:3]
        )
        
        
        # Add an output port (optional)
        self$addOutputPort(
          name = "selection",
          description = "data points selected from brushing",
          sample = mtcars[5:6,])
      })
      
      
    },
    ui = function(outputFunc=NULL, header = NULL) {
      super$ui(
        fluidRow(
          ifelse(is.null(outputFunc),
                 tagList(
                   plotOutput(self$ns("plot"), 
                              brush = self$ns("brush"))
                 ),
                 tagList(
                   outputFunc(self$ns("plot")),
                   self$ns("brush")  
                 )
          )
        ),
        header = header
      )
      
    },
    server = function(input, output, session){
      # Mandatory
      super$server(input,output,session)
      
      selection <- reactive({
        d<-self$getInput("data")
        data <- d()
        brushedPoints(data, input$brush, allRows = TRUE)
      })
      
      selectionOnly <- reactive({
        d<-self$getInput("data")
        data <- d()
        brushedPoints(data, input$brush, allRows = FALSE)
      })
      
      output$plot <- self$renderPlot(
        selection,
        self$getInput("mapping")
      )
      
      # Ports assignment starts here
      self$assignPort({
        
        self$updateOutputPort(
          id = "selection",
          output = selectionOnly)
        
      })
      
      return(selectionOnly)
    },
    renderPlot = function(data,cols){
      renderPlot({
        self$chart(data,cols)()
      })
    },
    aes = function(cols){
      shiny::req(length(cols) > 1)
      x <-  cols[1]
      y <- cols[2]
      g <- cols[3]
      
      aes <-  aes_string(x = x, y = y, group = g, color = g)
      if(is.na(g))
        aes <-  aes_string(x = x, y = y)
      
      return(aes)
    },
    chart = function(...){
      warning("charting function need to be implemented in a child class, SHOULD RETURN A REACTIVE FUNCTION!")
    }
  )
)


