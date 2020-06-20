
#' 
#' @title Retrieve module from ModStore
#'
#' @description This utility function retrieve tidymodules from the central ModStore
#' using module namespace/id and/or group
#' 
#' @param id Name or Id of the module
#' @param group Group name
#' 
#' @import shiny
#' 
#' @export
#'
#' @examples
#' 
#' MyModule <- R6::R6Class("MyModule", inherit = tidymodules::TidyModule)
#' MyModule$new("MyFirst")
#' MyModule$new("MySecond")
#' MyModule$new("MyThird",group = "B")
#' 
#' # MyFirst
#' getMod(1)
#' getMod("MyFirst")
#' 
#' # MySecond
#' getMod(2)
#' 
#' # MyThird
#' getMod(2)
#' getMod("B-MyThird")
#' getMod(1,group="B")
#'
getMod <- function(id = 1, group = NULL){
  m <- UtilityModule$new()
  mod <- NULL
  c <- isolate(m$getSession()$collection)
  gc <- isolate(m$getSession()$g_collection)
  
  if(!is.null(group) && !is.numeric(id))
    id <- paste0(id,"-G-",group)
  
  if(is.null(group)){
    mod <- c[[id]]
  }else{
    mod <- gc[[group]][[id]]
  }
  
  if(is.null(mod))
    warning(paste0("Module ",id," not found!"))
  
  mod
}
#' 
#' @title Alias to getMod
#'
#' @description See \code{\link{getMod}}
#' 
#' @param id Name or Id of the module
#' @param group Group name
#' 
#' @import shiny
#' 
#' @export
mod <- function(id = 1, group = NULL){
  getMod(id,group)
}

#' 
#' @title Retrieve module's port
#'
#' @description This utility function retrieve the tidymodules port specified in the arguments.
#' 
#' @param id Name or Id of the module
#' @param g Module group name
#' @param t Port type, in or out
#' @param p Port Id or name
#' 
#' @import shiny
#' 
#' @export
port <- function(id = 1, p = 1, t = "in", g = NULL){
  m <- getMod(id,g)
  if(is.null(m)){
    return(NULL)
  }else{
    if(t == "in")
      return(m$getInputPort(p))
    else
      return(m$getOutputPort(p))
  }
}
#' 
#' @title Retrieve input module's port
#'
#' @description This utility function retrieve the tidymodules input port specified in the arguments.
#' 
#' @param id Name or Id of the module
#' @param g Module group name
#' @param p Port Id or name
#' 
#' @import shiny
#' 
#' @export
iport <- function(id = 1, p = 1, g = NULL){
  port(id,p,"in",g)
}
#' 
#' @title Retrieve output module's port
#'
#' @description This utility function retrieve the tidymodules output port specified in the arguments.
#' 
#' @param id Name or Id of the module
#' @param g Module group name
#' @param p Port Id or name
#' 
#' @import shiny
#' 
#' @export
oport <- function(id = 1, p = 1, g = NULL){
  port(id,p,"out",g)
}

#' 
#' @title List modules in current session
#'
#' @description This function list module objects found in the current session
#' 
#' #' @param verbose Display module description as well
#' 
#' @importFrom  cli cat_bullet cat_boxx
#' @import shiny
#' 
#' @export
listModules <- function(verbose = FALSE, global = FALSE){
  currentSession <- UtilityModule$new()$getSession()
  isolate({
    if(length(currentSession$collection) == 0)
      cat_bullet(paste0("No module found!"),
                 bullet_col = "orange",
                 bullet = "cross")
    else
      cat_bullet(paste0("Found ",length(currentSession$collection)," module(s)!"),
                 bullet_col = "green",
                 bullet = "tick")
    invisible(for(mod in currentSession$collection) {
      cat_bullet(mod$module_ns,bullet = "circle_dotted")
      if(verbose)
        cat_boxx(capture.output(mod))
    })
  })
}

#' 
#' @title Call modules function
#'
#' @description This utility function call all modules initialized in the global session.
#' The global session is the session shared outside the server function of the application.
#' All the modules initialized in the global session can be called with this function in a single call.
#' The function take care of cloning and attaching them to the current user session.
#' 
#' Note that this function can only be called in the app server function at the moment.
#' We are working on supporting callModules within module server function for invoking nested modules.
#' 
#' 
#' @import shiny
#' 
#' @export
callModules <- function(){
  currentSession <- UtilityModule$new()$getSession()
  globalSession <- UtilityModule$new()$getGlobalSession()
  disable_cache <- getCacheOption()
  
  calls <- c()
  
  isolate({
    # re-initialize current session
    currentSession$edges <- data.frame()
    currentSession$count <- globalSession$count
    
    lapply(globalSession$collection,function(mod){
      if(is.null(currentSession$collection[[mod$module_ns]]) || disable_cache){
        ######## Try to capture server function arguments here ########
        serverEnv <- parent.frame(3)
        o <- i <- s <- NULL
        if(!is.null(serverEnv)){
          if(!is.null(serverEnv$input) &&
             is(serverEnv$input, "reactivevalues"))
            i <- serverEnv$input
          if(!is.null(serverEnv$output) &&
             is(serverEnv$output, "shinyoutput"))
            o <- serverEnv$output
          if(!is.null(serverEnv$session) &&
             is(serverEnv$session, "ShinySession"))
            s <- serverEnv$session
          if(is.null(s))
            s <- getDefaultReactiveDomain()
            
        }
        cloned <- mod$deepClone(o,i,s)
      }
      # Don't invoke nested modules as they will be invoked by parents
      # TODO : Change function to allow callModules within Module server (inject nested modules)
      if(is.null(currentSession$collection[[mod$module_ns]]$parent_ns))
        calls <<- c(calls,currentSession$collection[[mod$module_ns]])
    })
  })
  lapply(calls,function(m) m$callModule()) 
}
#' 
#' @title Function wrapper for ports connection expression.
#'
#' @description Used in server functions to define how modules are connected to each other.
#' 
#' @param x expression
#' 
#' @import shiny
#' 
#' @export
defineEdges  <- function(x){
  observe({
    isolate(x)
  })
}


#' 
#' @title Retrieve cache option from the environment
#'
#' @description The cache option `tm_disable_cache` is a global options that enable or disable the use of existing modules from the current session.
#' This option is `FALSE` by default and should be used in concordance with the `tm_session_type` global option. See \code{\link{session_type}} for a list of possible session type.
#' 
#' @export
getCacheOption <- function(){
  disable_cache = getOption("tm_disable_cache")
  if(is.null(disable_cache))
    disable_cache <- FALSE
  disable_cache <- as.logical(disable_cache)
  
  if(is.na(disable_cache))
    stop("Option 'tm_disable_cache' should be set to a logical value or unset.")
  
  disable_cache
}
#' 
#' @title List of possible session types
#'
#' @description tidymodules offers the ability to manage application sessions.
#' At the moment the three options below are available.
#' 
#' \itemize{
#' 
#' \item{SHINY}{ : The default behaviour of shiny application and the default for tidymodules. Every time you access an application
#' you get a new token Id that defines your application user session.}
#' 
#' \item{USER}{ : This method defines a session based on the information available in the request object of shiny output.
#' It is a concatenation of the variables REMOTE_ADDR, HTTP_HOST and PATH_INFO like below.
#' 
#' \code{sid <- paste0(r$REMOTE_ADDR,"@",r$HTTP_HOST,r$PATH_INFO))}
#' 
#' Note that the method is actually not working properly for now as the information available via the request object
#' are not reflecting the actual user. We are working on a better method to uniquely identify a remote user.}
#' 
#' \item{CUSTOM}{ : This method allow the developper to provide a custom function for generating the session Id.
#' It relies on the global options `tm_session_custom` being set and pointing to a function taking a shiny output as argument.}
#' 
#' }
#' 
#' @export
session_type <- list(
  SHINY = 1,
  USER  = 2,
  CUSTOM = 3
)

#' 
#' @title tidymodules options
#'
#' @name global_options
#'
#' @description List of global options used to adjust tidymodules configuration.
#' 
#' \itemize{
#' \item{**tm_session_type**}{ : Define the type of the session, See available session types in \code{\link{session_type}} }
#' \item{**tm_session_custom**}{ : Used to set a custom function for generating the session Id. Used in concordance with the `CUSTOM` session type.}
#' \item{**tm_disable_cache**}{ : Disable caching of modules. This option is set to FALSE by default but is only relevant when user's session is managed properly. See also \code{\link{getCacheOption}}}
#' }
#' 
#' @rdname  global_options
#' 
NULL

#' 
#' @title Function that generates session Id
#'
#' @description tidymodules offers the ability to manage application sessions.
#' This function is the main function used by tidymodules to find the current session Id.
#' It takes an optional ShinySession object as argument. If null, default to the global_session.
#' 
#' @param out A shiny output as provide by the shiny server function.
#' 
#' @return A session ID
#' 
#' @import shiny
#' 
#' @export
getSessionId <- function(session = getDefaultReactiveDomain()){
  if(is.null(session)){
    return("global_session")
  } else {
    stype <- getOption("tm_session_type")
    sid <- NULL
    if(is.null(stype))
      stype <- session_type$SHINY
    switch(stype,
       # SHINY
       {
         sid <- session$token
       },
       # USER
       {
         r <- session$request
         sid <- paste0(r$REMOTE_ADDR,"@",r$HTTP_HOST,r$PATH_INFO)
       },
       # CUSTOM
       {
         fct <- getOption("tm_session_custom")
         if(is.null(fct) || class(fct) != "function")
           stop("Option 'tm_session_custom' should be set to a function taking a ShinySession object as option and generating a custom session ID used by tidymodules to identify module sessions.")
         sid <- fct(session)
       }
    )
    return(sid)
  }
}


#' 
#' @title Recursive function for retrieving R6ClassGenerator inheritance
#'
#' @description This function is used to retrieve a list of class name that a R6ClassGenerator object inherit from.
#' 
#' @param r6cg A R6ClassGenerator object.
#' 
#' @return vector of class names
#' 
#' @keywords internal
get_R6CG_list <- function(r6cg){
  if(!is(r6cg,"R6ClassGenerator"))
    stop("provide a R6ClassGenerator object!")
  clist <- r6cg$classname
  if(!is.null(r6cg$get_inherit()))
    clist <- c(clist,get_R6CG_list(r6cg$get_inherit()))
  
  return(clist)
}
