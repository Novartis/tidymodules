
#' TidyModule R6 Class, parent of all modules
#'
#' @description
#' This is the main class provided by tidymodules and should be the parent of all tidymodules modules.
#' It provides methods for defining input and output communication ports, managing module namespace and many other useful features.
#'
#' @import shiny
#' @import R6
#' 
#' @export
TidyModule <- R6::R6Class(
  "TidyModule",
  public = list(
    #' @field id ID of the module.
    id = NULL,
    #' @field name Name of the module as generated or provided by the user.
    name = NULL,
    #' @field module_ns Module namespace, unique identifier for the module.
    module_ns = NULL,
    #' @field parent_ns Parent module namespace in case of nested modules.
    parent_ns = NULL,
    #' @field parent_mod Reference to parent module in case of nested modules.
    parent_mod = NULL,
    #' @field parent_ports logical value indicating if the module inherit the parent's input ports in case of nested modules.
    parent_ports = NULL,
    #' @field group Group name of the module.
    group = NULL,
    #' @field created Initialization time of the module.
    created = NULL,
    #' @field order Initialization order.
    order = NULL,
    # o = NULL,
    # i = NULL,
    #' @description
    #' Create a new tidymodules module.
    #' @param id Unique Id to assign to the module. Default to a generated Id using module's class and order of initialization.
    #' @param inherit logical value indicating if a nested module should inherits the parent's input ports. Default to TRUE
    #' @param group Module group name. Added to module's Id to make it unique. Optional 
    #' @return A new `TidyModule` object.
    initialize = function(id = NULL, inherit = TRUE, group = NULL) {
      
      #### Initialize ModStore #######
      if(is.null(private$shared$store))
        private$shared$store <- ModStore$new()
      
      self$parent_ports <- inherit
      
      ses <- self$getSession()
      shiny::isolate({
        ses$count <- ses$count+1
        self$order <- ses$count
      })
      shiny::onStop(function(){ 
        # reset session module count and edge table when app stop 
        ses$count <- 0
        ses$edges <- data.frame()
      })
      
      self$name <- ifelse(
        is.null(id),
        paste0(class(self)[[1]],"-",shiny::isolate({ses$count }))
        ,id)
      
      if(!is.null(group)){
        self$id <- paste0(group,"-",self$name)
        self$group <- group
      }else{
        self$id <- self$name
      }
      
      ################# Handle nested module here ################
      ############### Find and set the parent module #############
      # case 1 : nested module directly initialized in attribute definition of parent
      # This type of initialization should trigger an error
      # Because the parent module need to be initialized first
      # TODO : Find a way to prevent this....
      
      # if(class(sys.frame(9)$inherit) == "R6ClassGenerator")
      #   stop(paste0("Error in definition of module ",sys.frame(9)$classname,
      #              "! Nested module ",self$id," should be defined either in initialize() or server() of parent!"))
      
      # case 2 : 
      # for n = 2:3  => nested module initialized in initialze() or server() functions of parent
      # for n = 4:10 => same as before but support nested functions or module inheritance
      for(n in 2:10){
        self$parent_mod<-parent.env(parent.frame(n))$self
        if(!is.null(self$parent_mod) && is(self$parent_mod,"TidyModule"))
          break
      }
      # case 3 : Dynamically created module in an observe function of server()
      if(is.null(self$parent_mod) || !is(self$parent_mod,"TidyModule"))
        self$parent_mod<-parent.env(parent.env(parent.frame(3)))$self
      
      # At the end we save the parent ns & server arguments if any
      if(!is.null(self$parent_mod) && 
         is(self$parent_mod,"TidyModule")){
        self$parent_ns <- self$parent_mod$module_ns
        private$shiny_input   <- self$parent_mod$getShinyInput()
        private$shiny_output  <- self$parent_mod$getShinyOutput()
        private$shiny_session <- self$parent_mod$getShinySession()
      }else{
        self$parent_mod <- NULL
      }
      
      self$module_ns <- ifelse(
        is.null(self$parent_ns),
        self$id,
        paste0(self$parent_ns,"-",self$id))
      
      #### Try to capture server function arguments if not set #######
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
      
      # check that the module namespace is unique
      # if(self$isStored())
      #   stop(paste0("Module namespace collision with ",self$module_ns,", is it already used?"))
      
      self$created <- Sys.time()
      private$shared$store$addMod(self)
      private$initFields()
      
      # Only transfer parent ports in dynamic mode
      # i.e. when the child module is initialized on the server
      if(!self$isGlobal() &&
         !is.null(self$parent_mod) && 
         is(self$parent_mod,"TidyModule") &&
         self$parent_ports)
        self$parent_mod %:i:% self
    },
    #' @description
    #' namespace function used to generate unique Id for HTML elements.
    #' @param id Id of HTML element / shiny input.
    #' @return A unique string Id.
    ns = function(id){
      shiny::NS(self$module_ns, id)
    },
    #' @description
    #' Get module session Id. This function rely on a shiny output object to find the right session Id.
    #' @return The Session Id of the module.
    getSessionId = function(){
      return(getSessionId(private$shiny_output))
    },
    #' @description
    #' Alias to the `ui` function.
    #' @param ... arguments passed to the ui function.
    render = function(...){
      self$ui(...)
    },
    #' @description
    #' UI function generating the module HTML elements.
    #' This function is eventually implemented in the child module to give the module a visual representation.
    #' Please note that a module can have many visual representations.
    #' @param ... arguments passed to ui
    #' @return Empty tagList()
    ui = function(...){
      return(shiny::tagList())
    },
    #' @description
    #' server function to be overwritten and called by child module.
    #' @param input shiny input.
    #' @param output shiny output.
    #' @param session shiny session.
    server = function(input, 
                      output, 
                      session){
      # Need to isolate this block to avoid unecessary triggers
      shiny::isolate({
        private$shiny_session <- session
        private$shiny_input <- input
        private$shiny_output <- output
      })
    },
    #' @description
    #' Function wrapper for port definition expression.
    #' @param x expression
    definePort = function(x){
      shiny::isolate(x)
    },
    #' @description
    #' Function wrapper for port assignement expression.
    #' @param x expression
    assignPort = function(x){
      shiny::observe({
        shiny::isolate(x)
      })
    },
    #' @description
    #' Add input port function. To be called within `definePort` function
    #' @param name port name. must be a unique input port name.
    #' @param description port description.
    #' @param sample sample dataset consumed by this port. Mandatory!
    #' @param is_parent Is the port inherited from the parent module.
    #' @param input This argument should be ignored. Only here for backward compatibility.
    addInputPort = function(
      name = NULL,
      description = NULL,
      sample = NULL,
      input = FALSE,
      is_parent = FALSE){
      private$addPort(
        type = "input",
        name = name,
        description = description,
        sample = sample,
        is_parent = is_parent
      )
    },
    #' @description
    #' Function for filling an input port.
    #' Called within the `assignPort` function
    #' @param id Port name or Id .
    #' @param input The reacive object.
    updateInputPort = function(
      id = NULL,
      input = NULL){
      private$updatePort(id = id,port = input, type = "input")
    },
    #' @description
    #' This function add a set of input ports to the module.
    #' @param inputs reactivevalues with the input ports.
    updateInputPorts = function(
      inputs = NULL){
      private$updatePorts(ports = inputs, type = "input")
    },
    #' @description
    #' Get an input port from the module.
    #' @param id Name or Id of the port.
    #' @return A module input port. A reactivevalues object with name, description, sample, is_parent and port elements.
    getInputPort = function(id = 1){
      return(private$getPort(id,"input"))
    },
    #' @description
    #' Get all the input ports as a reactivevalues object.
    #' @return A reactivevalues object.
    getInputPorts = function(){
      return(private$getPorts("input"))
    },
    #' @description
    #' Get a input port slot.
    #' @param id Name or Id of the port.
    #' @return A reactive function or reactivevalues object.
    getInput = function(id = 1){
      return(private$get(id, "input"))
    },
    #' @description
    #' Function for filling an output port.
    #' Called within the `assignPort` function
    #' @param id Port name or Id .
    #' @param output The reacive object.
    updateOutputPort = function(
      id = NULL,
      output = NULL){
      private$updatePort(id = id,port = output, type = "output")
    },
    #' @description
    #' This function add a set of output ports to the module.
    #' @param outputs reactivevalues with the output ports.
    updateOutputPorts = function(
      outputs = NULL){
      private$updatePorts(ports = outputs, type = "output")
    },
    #' @description
    #' Add output port function. To be called within `definePort` function
    #' @param name port name. must be a unique output port name.
    #' @param description port description.
    #' @param sample sample dataset returned by this port. Mandatory!
    #' @param is_parent Is the port inherited from the parent module.
    #' @param output This argument should be ignored. Only here for backward compatibility.
    addOutputPort = function(
      name = NULL,
      description = NULL,
      sample = NULL,
      output = FALSE,
      is_parent = FALSE){
      private$addPort(
        type = "output",
        name = name,
        description = description,
        sample = sample,
        is_parent = is_parent
      )
    },
    #' @description
    #' Utility function that returns a port name from the Id.
    #' @param id Port Id
    #' @param type Port type, input or output.
    #' @return Port name.
    getPortName = function(id = NULL, type = "input"){
      port <- private$getPort(id,type)
      return(port$name)
    },
    #' @description
    #' Get an output port from the module.
    #' @param id Name or Id of the port.
    #' @return A module output port. A reactivevalues object with name, description, sample, is_parent and port elements.
    getOutputPort = function(id = 1){
      return(private$getPort(id,"output"))
    },
    #' @description
    #' Get all the output ports as a reactivevalues object.
    #' @return A reactivevalues object.
    getOutputPorts = function(){
      return(private$getPorts("output"))
    },
    #' @description
    #' Get a output port slot.
    #' @param id Name or Id of the port.
    #' @return A reactive function or reactivevalues object.
    getOutput = function(id = 1){
      return(private$get(id, "output"))
    },
    #' @description
    #' Function for retrieving the central ModStore.
    #' @return The `ModStore` object.
    getStore = function(){
      return(private$shared$store)
    },
    #' @description
    #' Utility function that counts the number of input ports.
    #' @return The input ports count.
    countInputPort = function(){
      return(private$countPort("input"))
    },
    #' @description
    #' Utility function that counts the number of output ports.
    #' @return The output ports count.
    countOutputPort = function(){
      return(private$countPort("output"))
    },    
    #' @description
    #' Alias to the `callModule` function.
    #' @param ... arguments passed to the `server` function of the module.
    use = function(...){
      self$callModule(...)
    },
    #' @description
    #' callModule function. Similar to shiny callModule. Used to inject the server logic into the application.
    #' This function don't need the user to provide a namespace Id as a module already knows its identity.
    #' Options provided as arguments will be passed to the server function of the module.
    #' Note that the module reference `self` might not be the one injected.
    #' @param ... arguments passed to the `server` function of the module.
    callModule = function(...){
      # arguments from server environment
      output <- parent.frame()$output
      input <- parent.frame()$output
      session <- parent.frame()$session
      disable_cache <- getCacheOption()
      
      if(!is.null(private$shiny_output)){
        self$doServer(...)
      }else{
        mod <- self$deepClone(output,input,session)
        
        shiny::isolate({
          currentSession <- mod$getSession()
          globalSession <- self$getGlobalSession()
          currentSession$edges <- data.frame()
          currentSession$count <- globalSession$count
        })
        
        if(!mod$isStored() || disable_cache){
          self$getStore()$addMod(mod)
          mod$doServer(...)
        }else{
          remove(mod)
          getMod(self$module_ns)$doServer(...)
        }
      }
    },
    #' @description
    #' Function to check if the module is store in the current session.
    #' @return logical value.
    isStored = function(){
      return(self$getStore()$isStored(self))
    },
    #' @description
    #' Check if the session attached to the module is the `global_session`.
    #' @return logical value.
    isGlobal = function(){
      if(self$getSessionId() == "global_session")
        return(TRUE)
      else
        return(FALSE)
    },
    #' @description
    #' Get the current session.
    #' @return A reactivevalues object with the following elements.
    #' aid = application Id
    #' path = application path
    #' sid = session Id
    #' count = current module count
    #' created = session creation time
    #' updated = session update time
    #' collection = list of session modules
    #' edges = list of connection edges
    getSession = function(){
      return(self$getStore()$getSession(self))
    },
    #' @description
    #' Get the global session.
    #' @return A reactivevalues object with the following elements.
    #' aid = application Id
    #' path = application path
    #' sid = "global_session"
    #' count = session module count
    #' created = session creation time
    #' updated = session update time
    #' collection = list of session modules
    #' edges = empty data.frame
    getGlobalSession = function(){
      return(self$getStore()$getGlobalSession())
    },
    #' @description
    #' Function interfacing with shiny's callModule.
    #' @param ... arguments passed to the `server` function of the module.
    doServer = function(...){
      shiny::callModule(self$server,self$id,...)
    },
    #' @description
    #' Utility function to retrieve a port definition in the form of a list.
    #' This is a useful function to learn about a specific port.
    #' @param type Port type, input or output.
    #' @param id Name or Id of port.
    #' @return List of the port definition.
    getPortDef = function(type = NULL, id = 1){
      port <- NULL
      if(is.null(type) || !type %in% c("input","output"))
        stop("type must be one of input/output")
      
      shiny::isolate({
        port <- private$getPort(id,type)
        shiny::reactiveValuesToList(port)
      })
      
    },
    #' @description
    #' Module printing function.
    #' Print the structure of a module.
    #' @examples 
    #' MyModule <- R6::R6Class("MyModule", inherit = tidymodules::TidyModule)
    #' m <- MyModule$new()
    #' m
    print = function(){
      shiny::isolate({
        cat(paste0("Module Namespace ",self$module_ns,"\n"))
        if(!is.null(self$group))
          cat(paste0("Module Group ",self$group,"\n"))
        cat(paste0("Module Session ",self$getSessionId(),"\n"))
        cat(paste0("- Class ",paste0(class(self),collapse = " << "),"\n"))
        cat(paste0("- Input [",self$countInputPort(),"]\n"))
        private$printPorts("input")
        cat(paste0("- Output [",self$countOutputPort(),"]\n"))
        private$printPorts("output")
      })
    },
    #' @description
    #' Module cloning function.
    #' Take care of ports (cloning reactive objects) and nested modules.
    #' Note that the Ids/namespace are not changed.
    #' @param o Optional shiny output.
    #' @param i Optional shiny input
    #' @param s Optional shiny input
    #' @return A cloned module. 
    deepClone = function(o = NULL, i = NULL, s = NULL){
      shiny::isolate({
        copy <- self$clone()
        copy$reset(o,i,s)
        
        for(type in c("input","output")){
          if(private$countPort(type) > 0)
            for(idx in 1:private$countPort(type)){
              po <- private$getPort(idx,type)
              # Don't add port inherited from parent module
              # They will be added by parent, see below...
              if(po$parent)
                next
              if(type == "input")
                copy$addInputPort(po$name,po$description,po$sample)
              else
                copy$addOutputPort(po$name,po$description,po$sample)
            }
        }
        
        # Now deep clone the module attributes that are TidyModules, i.e. nested modules
        # TODO : Add code to check list as well
        for(at in names(self)){
          classes <- class(self[[at]])
          if(length(classes) > 1 && 
             rev(classes)[[2]] == "TidyModule" &&
             at != "parent_mod"){
            copy[[at]] <- self[[at]]$deepClone(o,i,s)
            copy[[at]]$parent_mod <- copy
            # Now add ports to child if any
            if(copy[[at]]$parent_ports)
              copy %:i:% copy[[at]]
            self$getStore()$addMod(copy[[at]])
          }
        }
        
        copy$created <- Sys.time()
      })
      
      return(copy)
    },
    #' @description
    #' This function reset the ports.
    #' @param o Optional shiny output.
    #' @param i Optional shiny input
    #' @param s Optional shiny input
    reset = function(o = NULL, i = NULL, s = NULL){
      private$input_port <- reactiveValues()
      private$output_port <- reactiveValues()
      private$port_names <- reactiveValues()
      if(!is.null(o))
        private$shiny_output <- o
      if(!is.null(i))
        private$shiny_input <- i
      if(!is.null(s))
        private$shiny_session <- s
    },
    #' @description
    #' Retrieve the shiny input.
    #' @return Shiny input object.
    getShinyInput = function(){
      return(private$shiny_input)
    },
    #' @description
    #' Retrieve the shiny output.
    #' @return Shiny output object.
    getShinyOutput = function(){
      return(private$shiny_output)
    },
    #' @description
    #' Retrieve the shiny output.
    #' @return Shiny session object.
    getShinySession = function(){
      return(private$shiny_session)
    }
  ),
  private = list(
    shared = { e <- new.env(); e$store <- NULL ; e },
    shiny_input = NULL,
    shiny_output = NULL,
    shiny_session = NULL,
    input_port = NULL,
    output_port = NULL,
    port_names = NULL,
    initFields = function(){
      private$input_port <- shiny::reactiveValues()
      private$output_port <- shiny::reactiveValues()
      private$port_names <- shiny::reactiveValues()
    },
    countPort = function(type = "input"){
      key = paste0(type,"_port")
      return(length(names(private[[key]])))
    },
    addPort = function(
      type = "input",
      name = NULL,
      description = paste0("Short description for this module's ",type),
      sample = NULL,
      port = FALSE,
      is_parent = FALSE){
      stopifnot(!is.null(name) && !is.null(sample))
      rv <- shiny::reactiveValues(
        name = name,
        description = description,
        sample = sample,
        parent = is_parent
      )
      
      p = port
      attr(p,"tidymodules")  <- TRUE
      attr(p,"tidymodules_port_type")        <- type
      attr(p,"tidymodules_port_id")          <- private$countPort(type)+1
      attr(p,"tidymodules_port_name")        <- name
      attr(p,"tidymodules_port_description") <- description
      attr(p,"tidymodules_port_sample")      <- sample
      attr(p,"tidymodules_is_parent")        <- is_parent
      attr(p,"tidymodules_module_ns")        <- self$module_ns
      
      rv[["port"]] <- p
      private[[paste0(type,"_port")]][[name]] <- rv
      nv <- private$port_names[[type]]
      private$port_names[[type]] <- c(nv,name)
    },
    updatePort = function(id = NULL, port = NULL, type = "input"){
      stopifnot((!is.null(id)))
      
      if(!is.null(port)){
        if(!shiny::is.reactivevalues(port) &&
           !shiny::is.reactive(port) )
          stop(paste0(deparse(substitute(port))," is not reactive"))
        
        key = paste0(type,"_port")
        if(is.numeric(id)){
          if(!id %in% seq(1,private$countPort(type))){
            stop(paste0("Port Update Failure: Numeric ",type," port [",id,"] not found in Module definition"))
          }else{
            id<-private$port_names[[type]][id]
          }
        }
        if(is.character(id) && !id %in% names(private[[key]])){
          stop(paste0("Port Update Failure: Character ",type," port [",id,"] not found in Module definition"))
        }
        
          
        # Attach module information to the port
        # This will facilitate storage of module edges
        shiny::isolate({
          attrs <- attributes(private[[key]][[id]][["port"]])
          for(a in names(attrs))
            if(grepl("tidymodules",a))
              attr(port,a) <- attrs[[a]]
            
            private[[key]][[id]][["port"]] <- port
        })
      }
    },
    updatePorts = function(ports = NULL, type = "input"){
      stopifnot(!is.null(ports))
      if(!shiny::is.reactivevalues(ports))
        stop(paste0(deparse(substitute(ports))," is not a reactive expression"))
      
      key = paste0(type,"_port")
      
      shiny::isolate({
        for(p in names(ports)){
          if(p %in%  private$port_names[[type]]){
            stop(paste0("Adding port name ",p," failed, it already exist in ",type," port definition."))
          }else{
            port <- ports[[p]]
            private$addPort(type,port$name,port$description,port$sample,port$port,is_parent = TRUE)
          }
        }
      })
    },
    get = function(id = 1,type = "input"){
      data.port <- private$getPort(id, type)
      
      shiny::req(data.port)
      # shiny::req(data.port[[type]])

      return(data.port[["port"]])
    },
    getPort = function(id = 1,type = "input"){
      key = paste0(type,"_port")
      if(private$countPort(type) == 0){
        warning(paste0("Module ",self$module_ns," has no ",type," ports"))
        return(NULL)
      }
      if(is.numeric(id)){
        if(!id %in% seq(1,private$countPort(type))){
          warning(paste0("Numeric ",type," port [",id,"] not found in Module definition"))
          return(NULL)
        }else{
          id<-private$port_names[[type]][id]
        }
      }
      if(is.character(id) && !id %in% names(private[[key]])){
        warning(paste0("Character ",type," port [",id,"] not found in Module definition"))
        return(NULL)
      }
      return(private[[key]][[id]])
    },
    getPorts = function(type = "input"){
      key = paste0(type,"_port")
      if(private$countPort(type) == 0)
        return(NULL)

      return(private[[key]])
    },
    printPorts = function(type = "input"){
      if(private$countPort(type)>0)
        for (p in 1:private$countPort(type)) {
          port = private$getPort(p,type)
          cat(paste0("(",p,") ",port$name," => ",ifelse(
            is.reactivevalues(port[["port"]]) ||
            is.reactive(port[["port"]]) || 
            port[["port"]],"OK","Empty"),"\n"))
        }
    },
    trimParentNS = function(){
      if(!is.null(self$parent_ns)){
        return(
          sub(
            paste0(self$parent_ns,"-"),
            "",
            self$module_ns
          )
        )
      } else {
        return(self$module_ns)
      }
    }
  )
)
