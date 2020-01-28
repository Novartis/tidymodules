
#' TidyModule utility class
#'
#' @description
#' A module used by some of the utility functions to retrieve the ModStore object.
#'
#' @details
#' This utility module is a special TidyModule class that doesn't get registered in the ModStore.
#' It is used to retrieve ModStore objects, like sessions and modules.
#' @examples
#' # Print current session Id
#' UtilityModule$new()$getSessionId()
UtilityModule <- R6::R6Class(
  "UtilityModule",
  inherit = TidyModule,
  public = list(
    #' @description
    #' Initialize function.
    #' @return `UtilityModule` object.
    initialize = function(){
      if(is.null(private$shared$store))
        private$shared$store <- ModStore$new()
      
      
      #### Try to capture server function arguments #######
      for(i in 1:10){
        serverEnv <- parent.env(parent.frame(i))
        if(!is.null(serverEnv)){
          if(!is.null(serverEnv$input) &&
             is(serverEnv$output, "shinyoutput")){
            private$shiny_input <- serverEnv$input
            private$shiny_output <- serverEnv$output
            private$shiny_session <- serverEnv$session
            
            break
          }
        }
      }
      if(is.null(private$shiny_output)){
        serverEnv <- parent.frame(3)
        private$shiny_input <- serverEnv$input
        private$shiny_output <- serverEnv$output
        private$shiny_session <- serverEnv$session
      }
    }
  )
)