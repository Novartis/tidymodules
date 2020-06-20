
#' TidyModule utility class
#'
#' @description
#' A module used by some of the utility functions to retrieve the ModStore object.
#'
#' @details
#' This utility module is a special TidyModule class that doesn't get registered in the ModStore.
#' It is used to retrieve ModStore objects, like sessions and modules.
#' @examples
#' \dontrun{
#' # Print current session Id
#' UtilityModule$new()$getSessionId()
#' }
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
      
      #### Set Shiny Session Here  #######
      private$shiny_session <- shiny::getDefaultReactiveDomain()
    }
  )
)