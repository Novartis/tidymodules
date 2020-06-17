#' @title Launcher for the tidymodules examples
#'
#' @description Helper function to launch the tidymodules examples.
#' 
#' @param id Example ID. If null display list of examples with ID.
#' @param server boolean. Is this a server call?
#' @param options list of options to be passed to shinyApps or shinyDir
#' 
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'  showExamples(1)
#'
#' }
showExamples <- function(id = NULL, server = F,  options = NULL) {
  examples <- list.dirs(system.file(package = "tidymodules","shiny/examples"),recursive = F)
  if(is.null(id)){
    names(examples) <- 1:length(examples)
    basename(examples)
  }else{
    if(!is.numeric(id)){
      stop("Please provide a numeric value")
    }
    if(id > length(examples) || id < 1){
      stop("Wrong ID provided")
    }
    
    if(server){
      setwd(examples[id])
      if(!is.null(options))
        shiny::shinyAppDir(examples[id], options = options)
      else
        shiny::shinyAppDir(examples[id])
    }else{
      shiny::runApp(examples[id])
    }
  }
}

#' @title check if package namespace exist, load it or display relevant information
#'
#' @description Utility function for managing package dependencies for tidymodules examples
#' 
#' @param p character verctor of package names
#'
#' @export
#'
#' @examples
#'
#' check_and_load("ggplot2")
#'
check_and_load <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop(paste0("Package ",p," needed for this shiny example to work. Please install it."),
         call. = FALSE)
  }
  library(p,character.only = TRUE)
}
