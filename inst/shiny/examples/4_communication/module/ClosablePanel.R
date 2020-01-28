ClosablePanel <- R6::R6Class(
  "ClosablePanel", 
  inherit = Panel,
  public = list(
    # TODO : Change the way we close closable panel
    # Add server logic to remove the module
    ui = function(..., status = "default", header = NULL) {
      
      header <- tagList(
        header,
        tags$span(
          style = "float: right;cursor: pointer;",
          id = self$ns("close"),
          shiny::icon("times")
        )
      )
      content <- tagList(
        ...,
        tags$head(
          tags$script(
            type="text/javascript",
            paste0(
              "setTimeout(function(){ $('#",self$ns('close'),"').click(function(){
                $('#",self$module_ns,"').parent().parent().remove()
               }) }, 500);"  
            )
          )
        )
      )
      
      super$ui(
        status = status,
        header = header,
        content
      )
      
    }
  )
)