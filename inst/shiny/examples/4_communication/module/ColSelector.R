
ColSelector <- R6::R6Class(
  "ColSelector",
  inherit = Panel,
  public = list(
    initialize = function(...){
      super$initialize(...)
      self$definePort({
        self$addInputPort(
          name = "data",
          description = "Any data table",
          sample = data.frame(a = 1:4, b = 1:4))
        self$addOutputPort(
          name = "mapping",
          description = "dynamic list of column selections",
          sample = list(
            mapping1 = c("col1","col2","col3"),
            mapping2 = c("col2","col4")
          ))
      })
    },
    ui = function(){
      super$ui(
        status = "primary",
        shiny::actionButton(self$ns("add"),label = "Add mapping",icon = icon("plus")),
        br(),
        tags$div(id=self$ns("uio_selector"))
      )
    },
    server = function(input, output, session){
      # Mandatory
      super$server(input,output,session)
      
      reacs <- reactiveValues(
        current = NULL,
        cols = NULL,
        mapping = list()
      )
      
      observe({
        dataPort <- self$getInput("data")
        req(dataPort)
        d<-dataPort()
        reacs$mapping <- list()
        req(!is.null(d))
        reacs$cols <- colnames(d)
        reacs$current <- 1
        shiny::removeUI(
          selector =  paste0("#",self$ns("uio_selector div")),
          multiple = TRUE
        )
      })
      
      observeEvent(input$add,{
        d<-self$getInput("data")()
        req(!is.null(d))
        insertUI(
          selector = paste0("#",self$ns("uio_selector")),
          where = "beforeEnd",
          session = session,
          ui = tagList(
            selectizeInput(
              inputId = self$ns(paste0("mapping-",reacs$current)), 
              label = paste0("mapping-",reacs$current), 
              multiple = T, 
              choices = reacs$cols, 
              options = list(maxItems = 4L))
          )
        )
        
        reacs$mapping[[paste0("mapping-",reacs$current)]] <- c("")
        reacs$current <- reacs$current + 1
      })
      
      observe({
        key <- paste0("mapping-",reacs$current-1)
        observeEvent(input[[key]],{
          reacs$mapping[[key]] <- input[[key]]
        })
      })
      
      # Server logic
      # Ports assignment starts here
      self$assignPort({
        self$updateOutputPort(
          id = "mapping",
          output = reactive({ reacs$mapping }) )
      })
      
      return(reactive(reacs$mapping))
    }
  ),
  private = list(
    # any private functions?
  )
)


