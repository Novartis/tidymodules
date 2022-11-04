#' connect ports from two different modules
#'
#'
#' @title Ports mapping function
#'
#' @description This function maps a module's outpout port to another module's input port.
#'
#' @param leftModule The left module object
#' @param leftPort Port name or Id of the left module's output port
#' @param rightModule The right module object
#' @param rightPort Port name or Id of the right module's input port
#' @param reverse ligical value indicating which module to return. Default to FALSE, the right module
#'
#' @export
map_ports <- function(leftModule = NULL, leftPort = 1,
                      rightModule = NULL, rightPort = 1,
                      reverse = FALSE) {
  if (!is.numeric(leftPort)) {
    stop("Left port ID 'leftPort' should be numeric")
  }
  if (!is.numeric(rightPort)) {
    stop("Right port ID 'rightPort' should be numeric")
  }

  fct <- mkDoublePipe(leftPort, rightPort, rev = reverse)
  fct(leftModule, rightModule)
}


#' @title Combine ports function
#'
#' @description This function combines ports into a reactive list (reactiveValues)
#'
#' @param ... key/value pairs of ports
#'
#' @examples
#' \dontrun{
#' # Somewhere in the app...
#' MyModule <- R6::R6Class("MyModule", inherit = tidymodules::TidyModule)
#' MyModule$new("Mod1")
#' MyModule$new("Mod2")
#' MyModule$new("Mod3")
#'
#' # Must be in the server code and after calling the modules!
#' callModules()
#' observe({
#'   combine_ports(
#'     input_1 = mod(1)$getOutput(1),
#'     input_2 = mod(2)$getOutput(1)
#'   ) %>1% mod(3)
#' })
#' }
#'
#' @import shiny
#'
#' @export
combine_ports <- function(...) {
  args <- list(...)
  r <- NULL
  if (length(args)) {
    if (is.null(names(args))) {
      names(args) <- seq_len(length(args))
    }
    r <- do.call(reactiveValues, args)
  } else {
    r <- reactiveValues()
  }

  # Make this reactive aware of its tidymoduleness
  attr(r, "tidymodules") <- TRUE
  attr(r, "tidymodules_operation") <- "combine"

  return(r)
}

#'
#' @title Race ports function
#'
#' @description This function collapse ports into a single port and make them race (i.e. always return the last one updated)
#'
#' @param ... List of racing ports
#'
#' @examples
#' \dontrun{
#' # Somewhere in the app...
#' MyModule <- R6::R6Class("MyModule", inherit = tidymodules::TidyModule)
#' MyModule$new("Mod1")
#' MyModule$new("Mod2")
#' MyModule$new("Mod3")
#'
#' # Must be in the server code and after calling the modules!
#' callModules()
#' observe({
#'   race_ports(
#'     mod(1)$getOutput(1),
#'     mod(2)$getOutput(1)
#'   ) %>1% mod(3)
#' })
#' }
#'
#' @import shiny
#'
#' @export
race_ports <- function(...) {
  racers <- list(...)

  if (is.null(racers) || length(racers) == 0) {
    stop("In order to start a race, we need some ports!")
  }

  p <- length(racers) + 1
  r <- reactiveVal(
    label = "race", 
    value = reactive({ 
    })
  )

  lapply(seq_len(length(racers)), function(r) {
    reac <- racers[[r]]
    observeEvent(
      {
        reac()
      },
      {
        req(reac())
        r(reac)
      },
      priority = p
    )
    p <- p - 1
  })

  reactive_racer <- reactive({
    r()
    isolate({
      o <- r()
      o()
    })
  })

  # Make this reactive aware of its tidymoduleness
  attr(reactive_racer, "tidymodules") <- TRUE
  attr(reactive_racer, "tidymodules_operation") <- "race"

  return(reactive_racer)
}
