
PlotGenerator <- R6::R6Class(
  classname = "PlotGenerator", 
  inherit = Panel,
  public = list(
    plots = list(
      line = c("LinePlot","Xiao"),
      box = c("BoxPlot","David"),
      scatter = c("ScatterPlot","Mustapha"),
      scatter3D = c("Scatter3DPlot","Mustapha")
    ),
    initialize = function(...){
      # Mandatory
      super$initialize(...)
      
      # Ports definition starts here
      self$definePort({
        # At least one input
        self$addInputPort(
          name = "mappings",
          description = "Named list of character vectors defining mapping of columns",
          sample = list(
            map1 = c("col1","col2"),
            map2 = c("col4","col7")
          ))
        
        self$addInputPort(
          name = "tables",
          description = "Named list of data tables forwarded to the plotting modules",
          sample = list(
            raw = data.frame(id = 1:10,val = 11:20),
            filtered = data.frame(id = 5:10, val = 15:20)
          ))
        
        # Add an output port (optional)
        self$addOutputPort(
          name = "selection",
          description = "Data table with user selected data points",
          sample = head(mtcars))
      })
      
      
    },
    ui = function() {
      
      controls <- tagList(
        tags$br(),tags$br(),
        div(style="display: inline-block;vertical-align:middle; width: 150px;",shiny::selectInput(self$ns("data"),label = "data",choices = c())),
        div(style="display: inline-block;vertical-align:middle; width: 150px;",shiny::selectInput(self$ns("mapping"),label = "mapping",choices = c())),
        div(style="display: inline-block;vertical-align:middle; width: 150px;",shiny::selectInput(self$ns("plot"),label = "plot type",choices = names(self$plots))),
        div(style="display: inline-block;vertical-align:middle; width: 130px;",shiny::checkboxInput(self$ns("disconect_data"),label = "disconnect data", value = TRUE)),
        div(style="display: inline-block;vertical-align:middle; width: 130px;",shiny::checkboxInput(self$ns("disconect_mapping"),label = "disconnect mapping", value = TRUE)),
        div(style="display: inline-block;vertical-align:middle; width: 150px;",shiny::actionButton(self$ns("add"),label = NULL,icon = icon("plus")))
      )
      
      super$ui(
        status = "success",
        header = controls,
        fluidRow(
          id = self$ns("plotContainer")
        )
      )
      
    },
    server = function(input, output, session){
      # Mandatory
      super$server(input,output,session)
      
      reacs <- reactiveValues(
        brushing = NULL
      )
      
      observe({
        t <- self$getInput("tables")
        shiny::req(t)
        shiny::updateSelectInput(session,"data",choices = names(t),selected = "raw")
      })
      
      observe({
        mPort <- self$getInput("mappings")
        req(mPort)
        options <- names(mPort())
        if(is.null(options))
          options <- list()
        shiny::updateSelectInput(session,"mapping",choices = options)
      })
      
      observeEvent(input$add,{
        
        reactive_mapping <- self$getInput("mappings")
        reactive_table   <- self$getInput("tables")
        
        selected_mapping  <- input$mapping
        selected_data     <- input$data
        selected_plot     <- input$plot
        
        req(reactive_mapping)
        req(reactive_table)
        
        current_mapping <- reactive_mapping()[[selected_mapping]]
        req(length(current_mapping) != 0)
        
        current_data <- reactive_table[[selected_data]]()
        req(!is.null(current_data))
        
        mod     <- self$plots[[selected_plot]][1]
        author  <- self$plots[[selected_plot]][2]
        mod     <- eval(parse(text = mod))
        
        # dynamically create the selected charting module
        # Note that these are nested modules (module namespace includes Doug_)
        mod     <- mod$new(paste0(author,"_",input$add))
        
        # feed a static version of the data/mapping to the module
        if(input$disconect_data)
          reactive(current_data) %->% mod$iport("data")
        else
          reactive_table[[selected_data]] %->% mod$iport("data")
        
        if(input$disconect_mapping)
          reactive(current_mapping) %->% mod$iport("mapping")
        else
          reactive({ reactive_mapping()[[selected_mapping]] }) %->% mod$iport("mapping")
        
        # now call the module 
        mod$callModule()
        
        mflag <- tagList(shiny::icon("bolt")," ")
        if(input$disconect_mapping)
          mflag <- ""
        
        dflag <- tagList(shiny::icon("bolt")," ")
        if(input$disconect_data)
          dflag <- ""
        
        header <- tagList(" - ",dflag,selected_data," - ",mflag,selected_mapping)
        
        # render the module
        insertUI(
          selector = paste0("#",self$ns("plotContainer")),
          where = "afterBegin",
          immediate = TRUE,
          session = session,
          ui = tagList(
            column(6,mod$ui(header = header))
           )
         )
      })
      
      # Ports assignment starts here
      self$assignPort({
        # TODO: Add output port, e.g. brushing selection ?
        # self$updateOutputPort(
        #   id = "dataset",
        #   output = modOut)
        
      })
      
      return({})
      
    }
  )
)


