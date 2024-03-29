
#' TidyModule R6 Class, parent of all modules
#'
#' @description
#' This is the main class provided by tidymodules and should be the parent of all tidymodules modules.
#' It provides methods for defining input and output communication ports, managing module namespace and many other useful features.
#'
#' @import R6
#' @import shiny
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
    #' @field pass_ports logical value indicating if the module should pass its ports to any nested modules. This is initialized with the `inherit` argument of `new()`.
    pass_ports = NULL,
    #' @field group Group name of the module.
    group = NULL,
    #' @field created Initialization time of the module.
    created = NULL,
    #' @field order Initialization order.
    order = NULL,
    #' @field i reactive list of input ports.
    i = NULL,
    #' @field o reactive list of output ports.
    o = NULL,
    #' @field port_names list of input and output port names.
    port_names = NULL,
    #' @field react list of reactive objects used by the module.
    react = list(),
    #' @field obser list of observers used by the module.
    obser = list(),
    #' @field suspended boolean indicating whether the module observers are suspended.
    suspended = FALSE,
    #' @description
    #' Create a new tidymodules module.
    #' @param id Unique Id to assign to the module. Default to a generated Id using module's class and order of initialization.
    #' @param inherit logical value indicating if a nested module should inherits the parent's input ports. Default to TRUE
    #' @param group Module group name. Added to module's Id to make it unique. Optional
    #' @param parent Parent module. Need to be specified when creating a dynamic nested module. Optional
    #' @param collision Allow module id collision. Default to false.
    #' Id collision happens when you try to creating the same module (same Id) twice at the same time.  Optional
    #' @return A new `TidyModule` object.
     initialize = function(id = NULL, inherit = TRUE, group = NULL, parent = NULL, collision = FALSE) {
      #### Initialize ModStore #######
      if (is.null(private$shared$store)) {
        private$shared$store <- ModStore$new()
      }

      self$pass_ports <- inherit

      ses <- self$getSession()
      isolate({
        ses$count <- ses$count + 1
        self$order <- ses$count
      })
      onStop(function() {
        # reset session module count and edge table when app stop
        ses$count <- 0
        ses$edges <- private$emtpy_edges
      })

      self$name <- ifelse(
        is.null(id),
        paste0(class(self)[[1]], "-", isolate({
          ses$count
        })),
        private$sanitizeID(id, "Id")
      )

      if (!is.null(group)) {
        g <- private$sanitizeID(group, "Group")
        self$id <- paste0(g, "-", self$name)
        self$group <- g
      } else {
        self$id <- self$name
      }

      ################# Handle nested module here ################
      ############### Find and set the parent module #############

      # case 1 : if parent module is specified.
      if (!is.null(parent) && is(parent, "TidyModule")) {
        self$parent_mod <- parent
      }


      # case 2 : nested module directly initialized in attribute definition of parent
      # This type of initialization should trigger an error
      # Because the parent module need to be initialized first
      # TODO : Find a way to prevent this....

      # if(class(sys.frame(9)$inherit) == "R6ClassGenerator")
      #   stop(paste0("Error in definition of module ",sys.frame(9)$classname,
      #              "! Nested module ",self$id," should be defined either in initialize() or server() of parent!"))

      # case 3 :
      # for n = 2:3  => nested module initialized in initialze() or server() functions of parent
      # for n = 4:10 => same as before but support nested functions or module inheritance
      if (is.null(self$parent_mod)) {
        for (n in 2:10) {
          self$parent_mod <- parent.env(parent.frame(n))$self
          if (!is.null(self$parent_mod) && is(self$parent_mod, "TidyModule")) {
            break
          }
        }
      }
      # case 4 : Dynamically created module in an observe function of server()
      if (is.null(self$parent_mod) || !is(self$parent_mod, "TidyModule")) {
        self$parent_mod <- parent.env(parent.env(parent.frame(3)))$self
      }

      # At the end we save the parent ns & server arguments if any
      if (!is.null(self$parent_mod) &&
        is(self$parent_mod, "TidyModule")) {
        self$parent_ns <- self$parent_mod$module_ns
      } else {
        self$parent_mod <- NULL
      }

      self$module_ns <- ifelse(
        is.null(self$parent_ns),
        self$id,
        paste0(self$parent_ns, "-", self$id)
      )

      ####   Capture ShinySession    #######
      private$shiny_session <- shiny::getDefaultReactiveDomain()

      self$created <- Sys.time()

      # check that the module namespace is unique in the current session
      if (self$isStored() &&
        as.character(mod(self$module_ns)$created) == as.character(self$created) &&
        !collision) {
        stop(paste0(
          "Module namespace collision!\n",
          "Make sure that the namespace Id ", self$module_ns, " is only used once."
        ))
      }

      private$shared$store$addMod(self)
      private$initFields()

      # Only transfer parent ports in dynamic mode
      # i.e. when the child module is initialized on the server
      if (!self$isGlobal() &&
        !is.null(self$parent_mod) &&
        is(self$parent_mod, "TidyModule") &&
        self$pass_ports) {
        self$parent_mod %:pi:% self
      }
      # Handle debug mode
      if(
        !is.null(getOption("TM_DEBUG")) && 
        !is.na(as.logical(getOption("TM_DEBUG"))) &&
        as.logical(getOption("TM_DEBUG"))
        ){
        private$debug <- paste0("debug-",10e7*runif(1))
      }
      # Rewrite ui function and add a debug button
      if(!is.null(private$debug)){
        unlockBinding("ui",self$.__enclos_env__$self)
        ui_function <- self$ui
        self$ui <- function(...){
          data <- ui_function(...)
          debug <- actionButton(
            self$ns(private$debug),
            label = "", 
            icon = icon("bug","fa-small",style = "color:red;"), 
            class = "btn-sm",
            title = capture.output(self$print()))
          # Don't break tab-panel
          if(!is.null(data$attribs$class) &&
             data$attribs$class == "tab-pane"){
            data$children <- c(list(debug),data$children)
            data$attribs$style <- ifelse(
              is.null(data$attribs$style),
              "outline: 1px solid red;",
              paste0(data$attribs$style,";outline: 1px solid red;")
            )
            
          }else
            data <- tags$div(tagList(debug, data),style ="outline: 1px solid red;")
          
          data
        }
        lockBinding("ui",self$.__enclos_env__$self)
      }
    },
    #' @description
    #' namespace function used to generate unique Id for HTML elements.
    #' @param id Id of HTML element / shiny input.
    #' @return A unique string Id.
    ns = function(id) {
      NS(self$module_ns, id)
    },
    #' @description
    #' Get module session Id. This function rely on a shiny output object to find the right session Id.
    #' @return The Session Id of the module.
    getSessionId = function() {
      return(getSessionId(private$shiny_session))
    },
    #' @description
    #' Alias to the `ui` function.
    #' @param ... arguments passed to the ui function.
    render = function(...) {
      self$ui(...)
    },
    #' @description
    #' UI function generating the module HTML elements.
    #' This function is eventually implemented in the child module to give the module a visual representation.
    #' Please note that a module can have many visual representations.
    #' @param ... arguments passed to ui
    #' @return Empty tagList()
    ui = function(...) {
      return(tagList())
    },
    #' @description
    #' server function to be overwritten and called by child module.
    #' @param input shiny input.
    #' @param output shiny output.
    #' @param session shiny session.
    #' @param ... extra arguments to passed to the server function.
    server = function(input,
                      output,
                      session,
                      ...) {
      # Need to isolate this block to avoid unecessary triggers
      isolate({
        private$shiny_session <- session
        private$shiny_input <- input
        private$shiny_output <- output
      })
      # Handle debug
      if(!is.null(private$debug))
        self$obser$tm_debug <- observeEvent(input[[private$debug]],{
          browser()
        },ignoreInit = TRUE)
    },
    #' @description
    #' Preview the module in a gadget.
    #' @param ... extra arguments to passed to the server function.
    view = function(...) {
      s <- function(input, output, session) {
        self$server(input, output, session, ...)
      }

      app <- shinyApp(ui = miniUI::miniPage(self$ui()), server = s)
      # viewer <- dialogViewer(paste0("Preview of ",self$module_ns))
      viewer <- paneViewer()
      runGadget(app, viewer = viewer, stopOnCancel = TRUE)
    },
    #' @description
    #' Function wrapper for port definition expression.
    #' @param x expression
    definePort = function(x) {
      isolate(x)
    },
    #' @description
    #' Function wrapper for port assignment expression.
    #' @param x expression
    assignPort = function(x) {
      # TODO: There must be a better way to do that
      # Only add observe in non-reactive context
      if (is.null(shiny:::.getReactiveEnvironment()$.currentContext)) {
        observe({
          isolate(x)
        })
      } else {
        isolate(x)
      }
    },
    #' @description
    #' Add input port function. To be called within `definePort` function
    #' @param name port name. must be a unique input port name.
    #' @param description port description.
    #' @param sample sample dataset consumed by this port. Mandatory!
    #' @param is_parent Is the port inherited from the parent module.
    #' @param inherit Should the port be passed to nested module. default to TRUE.
    #' @param input This argument should be ignored. Only here for backward compatibility.
    addInputPort = function(name = NULL,
                            description = NULL,
                            sample = NULL,
                            input = FALSE,
                            is_parent = FALSE,
                            inherit = TRUE) {
      private$addPort(
        type = "input",
        name = name,
        description = description,
        sample = sample,
        is_parent = is_parent,
        inherit = inherit
      )
    },
    #' @description
    #' Function for filling an input port.
    #' Called within the `assignPort` function
    #' @param id Port name or Id .
    #' @param input The reacive object.
    updateInputPort = function(id = NULL,
                               input = NULL) {
      private$updatePort(id = id, port = input, type = "input")
    },
    #' @description
    #' This function add a set of input ports to the module.
    #' @param inputs reactivevalues with the input ports.
    #' @param is_parent Are the ports from a parent module. Default to FALSE.
    updateInputPorts = function(inputs = NULL,
                                is_parent = FALSE) {
      private$updatePorts(ports = inputs, type = "input", is_parent = is_parent)
    },
    #' @description
    #' Get an input port from the module.
    #' @param id Name or Id of the port.
    #' @return A module input port. A reactivevalues object with name, description, sample, is_parent and port elements.
    getInputPort = function(id = 1) {
      return(private$getPort(id, "input"))
    },
    #' @description
    #' Alias to the `getInputPort()` function.
    #' @param id Name or Id of the port.
    #' @return A module input port. A reactivevalues object with name, description, sample, is_parent and port elements.
    iport = function(id = 1) {
      return(self$getInputPort(id))
    },
    #' @description
    #' Get all the input ports as a reactivevalues object.
    #' @return A reactivevalues object.
    getInputPorts = function() {
      return(private$getPorts("input"))
    },
    #' @description
    #' Get a input port slot.
    #' @param id Name or Id of the port.
    #' @param w boolean to enable module session check. default to TRUE.
    #' @return A reactive function.
    getInput = function(id = 1, w = TRUE) {
      if (w && self$isGlobal() && !UtilityModule$new()$isGlobal()) {
        cmd <- paste0('mod("', self$module_ns, '")$getInput(', ifelse(is.character(id), paste0('"', id, '"'), id), ")")
        warning(paste(
          "You are trying to access a global session module port from a user session.",
          "Is this intended? If not, use the code below instead.",
          cmd,
          sep = "\n"
        ))
      }

      r <- private$get(id)
      # special handling for inherited input ports
      # since they are wraped in a reactive
      if (private$getPort(id)$parent) {
        r <- r()
      }

      return(r)
    },
    #' @description
    #' Execute an input port slot, that is, the reactive function stored in the port.
    #' The `require` argument which is `TRUE` by default allows you disable checking if the port is Truthy.
    #' See `shiny::req` function.
    #' @param id Name or Id of the port.
    #' @param require Check that the port is available.
    #' @return Output of the reacive function execution.
    execInput = function(id = 1, require = TRUE) {
      mod <- getMod(self$module_ns)
      r <- mod$getInput(id)
      if (require) {
        req(r)
      }
      return(r())
    },
    #' @description
    #' Function for filling an output port.
    #' Called within the `assignPort` function
    #' @param id Port name or Id .
    #' @param output The reacive object.
    updateOutputPort = function(id = NULL,
                                output = NULL) {
      private$updatePort(id = id, port = output, type = "output")
    },
    #' @description
    #' This function add a set of output ports to the module.
    #' @param outputs reactivevalues with the output ports.
    #' @param is_parent Are the ports from a parent module. Default to FALSE.
    updateOutputPorts = function(outputs = NULL,
                                 is_parent = FALSE) {
      private$updatePorts(ports = outputs, type = "output", is_parent = is_parent)
    },
    #' @description
    #' Add output port function. To be called within `definePort` function
    #' @param name port name. must be a unique output port name.
    #' @param description port description.
    #' @param sample sample dataset returned by this port. Mandatory!
    #' @param is_parent Is the port inherited from the parent module.
    #' @param output This argument should be ignored. Only here for backward compatibility.
    addOutputPort = function(name = NULL,
                             description = NULL,
                             sample = NULL,
                             output = FALSE,
                             is_parent = FALSE) {
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
    getPortName = function(id = NULL, type = "input") {
      port <- private$getPort(id, type)
      return(port$name)
    },
    #' @description
    #' Get an output port from the module.
    #' @param id Name or Id of the port.
    #' @return A module output port. A reactivevalues object with name, description, sample, is_parent and port elements.
    getOutputPort = function(id = 1) {
      return(private$getPort(id, "output"))
    },
    #' @description
    #' Alias to the `getOutputPort()` function.
    #' @param id Name or Id of the port.
    #' @return A module output port. A reactivevalues object with name, description, sample, is_parent and port elements.
    oport = function(id = 1) {
      return(self$getOutputPort(id))
    },
    #' @description
    #' Get all the output ports as a reactivevalues object.
    #' @return A reactivevalues object.
    getOutputPorts = function() {
      return(private$getPorts("output"))
    },
    #' @description
    #' Get a output port slot.
    #' @param id Name or Id of the port.
    #' @param w boolean to enable module session check. default to TRUE.
    #' @return A reactive function.
    getOutput = function(id = 1, w = TRUE) {
      if (w && self$isGlobal() && !UtilityModule$new()$isGlobal()) {
        cmd <- paste0('mod("', self$module_ns, '")$getOutput(', ifelse(is.character(id), paste0('"', id, '"'), id), ")")
        warning(paste(
          "You are trying to access a global session module port from a user session.",
          "Is this intended? If not, use the code below instead.",
          cmd,
          sep = "\n"
        ))
      }

      return(private$get(id, "output"))
    },
    #' @description
    #' Execute an output port slot, that is, the reactive function stored in the port.
    #' The `require` argument which is `TRUE` by default allows you disable checking if the port is Truthy.
    #' See `shiny::req` function.
    #' @param id Name or Id of the port.
    #' @param require Check that the port is available.
    #' @return Output of the reacive function execution.
    execOutput = function(id = 1, require = TRUE) {
      mod <- getMod(self$module_ns)
      r <- mod$getOutput(id)
      if (require) {
        req(r)
      }
      return(r())
    },
    #' @description
    #' Function for retrieving the central ModStore.
    #' @return The `ModStore` object.
    getStore = function() {
      return(private$shared$store)
    },
    #' @description
    #' Utility function that counts the number of input ports.
    #' @return The input ports count.
    countInputPort = function() {
      return(private$countPort("input"))
    },
    #' @description
    #' Utility function that counts the number of output ports.
    #' @return The output ports count.
    countOutputPort = function() {
      return(private$countPort("output"))
    },
    #' @description
    #' Alias to the `callModule` function.
    #' @param ... arguments passed to the `server` function of the module.
    use = function(...) {
      self$callModule(...)
    },
    #' @description
    #' callModule function. Similar to shiny callModule. Used to inject the server logic into the application.
    #' This function don't need the user to provide a namespace Id as a module already knows its identity.
    #' Options provided as arguments will be passed to the server function of the module.
    #' Note that the module reference `self` might not be the one injected.
    #' @param ... arguments passed to the `server` function of the module.
    callModule = function(...) {
      # arguments from server environment
      output <- parent.frame()$output
      input <- parent.frame()$output
      session <- parent.frame()$session
      if (is.null(session)) {
        session <- shiny::getDefaultReactiveDomain()
      }

      disable_cache <- getCacheOption()

      if (!self$isGlobal()) {
        self$doServer(...)
      } else {
        isolate({
          currentSession <- UtilityModule$new()$getSession()
          globalSession <- UtilityModule$new()$getGlobalSession()
          currentSession$edges <- private$emtpy_edges
          currentSession$count <- globalSession$count
          cloneMod <- is.null(currentSession$collection[[self$module_ns]])
        })

        if (cloneMod || disable_cache) {
          mod <- self$deepClone(output, input, session)
          mod$doServer(...)
        } else {
          getMod(self$module_ns)$doServer(...)
        }
      }
    },
    #' @description
    #' Function to check if the module is store in the current session.
    #' @return logical value.
    isStored = function() {
      return(self$getStore()$isStored(self))
    },
    #' @description
    #' Check if the session attached to the module is the `global_session`.
    #' @return logical value.
    isGlobal = function() {
      if (self$getSessionId() == "global_session") {
        return(TRUE)
      } else {
        return(FALSE)
      }
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
    getSession = function() {
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
    getGlobalSession = function() {
      return(self$getStore()$getGlobalSession())
    },
    #' @description
    #' Function interfacing with shiny's callModule.
    #' @param ... arguments passed to the `server` function of the module.
    doServer = function(...) {
      callModule(self$server, self$id, ...)
    },
    #' @description
    #' Utility function to retrieve a port definition in the form of a list.
    #' This is a useful function to learn about a specific port.
    #' @param type Port type, input or output.
    #' @param id Name or Id of port.
    #' @return List of the port definition.
    getPortDef = function(type = NULL, id = 1) {
      port <- NULL
      if (is.null(type) || !type %in% c("input", "output")) {
        stop("type must be one of input/output")
      }

      isolate({
        port <- private$getPort(id, type)
        reactiveValuesToList(port)
      })
    },
    #' @description
    #' Module printing function.
    #' Print the structure of a module.
    #' @examples
    #' MyModule <- R6::R6Class("MyModule", inherit = tidymodules::TidyModule)
    #' m <- MyModule$new()
    #' m
    print = function() {
      isolate({
        cat(paste0("Module Namespace ", self$module_ns, "\n"))
        if (!is.null(self$group)) {
          cat(paste0("Module Group ", self$group, "\n"))
        }
        cat(paste0("Module Session ", self$getSessionId(), "\n"))
        cat(paste0("- Class ", paste0(class(self), collapse = " << "), "\n"))
        cat(paste0("- Input [", self$countInputPort(), "]\n"))
        private$printPorts("input")
        cat(paste0("- Output [", self$countOutputPort(), "]\n"))
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
    deepClone = function(o = NULL, i = NULL, s = NULL) {
      isolate({
        copy <- self$clone()
        copy$reset(o, i, s)

        for (type in c("input", "output")) {
          if (private$countPort(type) > 0) {
            for (idx in 1:private$countPort(type)) {
              po <- private$getPort(idx, type)
              # Don't add port inherited from parent module
              # They will be added by parent, see below...
              if (po$parent) {
                next
              }
              if (type == "input") {
                copy$addInputPort(
                  name = po$name,
                  description = po$description,
                  sample = po$sample,
                  inherit = po$inherit
                )
              } else {
                copy$addOutputPort(
                  name = po$name,
                  description = po$description,
                  sample = po$sample
                )
              }
            }
          }
        }

        copy$created <- Sys.time()
        self$getStore()$addMod(copy)

        # Now deep clone the module attributes (also check list) that are TidyModules, i.e. nested modules
        for (at in names(self)) {
          if (is(self[[at]], "TidyModule") &&
            at != "parent_mod") { # When module attribute is a nested module
            copy[[at]] <- self[[at]]$deepClone(o, i, s)
            copy[[at]]$parent_mod <- copy
            self$getStore()$addMod(copy[[at]])
            # Now add ports to child if any
            if (copy[[at]]$pass_ports) {
              copy %:pi:% copy[[at]]
            }
          } else if (is.list(self[[at]]) &&
            length(copy[[at]]) > 0 &&
            !at %in% c("port_names", "i", "o")) { # Check list for modules
            l <- self[[at]]
            for (k in seq_len(length(l))) {
              if (is(l[[k]], "TidyModule")) {
                l[[k]] <- l[[k]]$deepClone(o, i, s)
                l[[k]]$parent_mod <- copy
                self$getStore()$addMod(l[[k]])
                # Now add ports to child if any
                if (l[[k]]$pass_ports) {
                  copy %:pi:% l[[k]]
                }
              }
            }
            copy[[at]] <- l
          }
        }
      })

      return(copy)
    },
    #' @description
    #' This function reset the ports.
    #' @param o Optional shiny output.
    #' @param i Optional shiny input
    #' @param s Optional shiny input
    reset = function(o = NULL, i = NULL, s = NULL) {
      private$initFields()
      if (!is.null(o)) {
        private$shiny_output <- o
      }
      if (!is.null(i)) {
        private$shiny_input <- i
      }
      if (!is.null(s)) {
        private$shiny_session <- s
      }
    },
    #' @description
    #' This function destroys the module by removing it from the `ModStore`
    #' and by destroying all its observers. Please note that this function 
    #' works properly only if the observers created and used by the module are 
    #' added to `self$obser`.
    #' @param deep optional logical indicating whether to destroy the
    #' child modules as well. default to FALSE.
    destroy = function(deep = FALSE){
      private$shared$store$delMod(self)
      lapply(self$obser,function(x) x$destroy())
      if(deep){
        for(m in self$getSession()$collection)
          # Child module namespace always starts with parent's namespace 
          if(grepl(paste0("^",self$module_ns),m$module_ns))
            m$destroy()
      }
    },
    #' @description
    #' This function suspends the module's observers. 
    #' Please note that this function works properly only if the observers 
    #' created and used by the module are added to `self$obser`.
    suspend = function(){
      lapply(self$obser,function(x) x$suspend())
      self$suspended <- TRUE
    },
    #' @description
    #' This function resumes the module's observers. 
    #' Please note that this function works properly only if the observers 
    #' created and used by the module are added to `self$obser`.
    resume = function(){
      lapply(self$obser,function(x) x$resume())
      self$suspended <- FALSE
    },
    #' @description
    #' Retrieve the shiny input.
    #' @return Shiny input object.
    getShinyInput = function() {
      return(private$shiny_input)
    },
    #' @description
    #' Retrieve the shiny output.
    #' @return Shiny output object.
    getShinyOutput = function() {
      return(private$shiny_output)
    },
    #' @description
    #' Retrieve the shiny output.
    #' @return Shiny session object.
    getShinySession = function() {
      return(private$shiny_session)
    }
  ),
  private = list(
    shared = {
      e <- new.env()
      e$store <- NULL
      e
    },
    shiny_input = NULL,
    shiny_output = NULL,
    shiny_session = NULL,
    emtpy_edges = data.frame(
      from = NA,
      fclass = NA,
      fport = NA,
      ftype = NA,
      fname = NA,
      to = NA,
      tclass = NA,
      tport = NA,
      ttype = NA,
      tname = NA,
      mode = NA,
      comment = NA
    )[numeric(0), ],
    debug = NULL,
    initFields = function() {
      self$i <- reactiveValues()
      self$o <- reactiveValues()
      self$port_names <- reactiveValues()
    },
    sanitizeID = function(id, type) {
      if (!grepl("[a-z][\\w-]*", id, ignore.case = TRUE, perl = TRUE) ||
        grepl('[>~!@\\$%\\^&\\*\\(\\)\\+=,./\';:"\\?><\\[\\]\\\\{}\\|`#]', id, ignore.case = TRUE, perl = TRUE)) {
        stop(paste(paste0("The provided ", type, " must begin with a letter `[A-Za-z]` and may be followed by any number of letters, digits `[0-9]`, hyphens `-`, underscores `_`."),
          "You should not use the following characters as they have a special meaning on the UI ( CSS / jQuery ).",
          "> ~ ! @ $ % ^ & * ( ) + = , . / ' ; : \" ? > < [ ] \ { } | ` #",
          sep = "\n"
        ))
      }

      return(id)
    },
    countPort = function(type = "input") {
      key <- ifelse(type == "input", "i", "o")
      return(length(names(self[[key]])))
    },
    addPort = function(type = "input",
                       name = NULL,
                       description = paste0("Short description for this module's ", type),
                       sample = NULL,
                       port = FALSE,
                       is_parent = FALSE,
                       inherit = TRUE) {
      stopifnot(!is.null(name) && !is.null(sample))
      rv <- reactiveValues(
        name = name,
        description = description,
        sample = sample,
        parent = is_parent,
        inherit = inherit
      )

      p <- port

      attr(rv, "tidymodules") <- TRUE
      attr(rv, "tidymodules_port_def") <- TRUE
      attr(rv, "tidymodules_port_type") <- type
      attr(rv, "tidymodules_port_id") <- private$countPort(type) + 1
      attr(rv, "tidymodules_port_name") <- name
      attr(rv, "tidymodules_port_description") <- description
      attr(rv, "tidymodules_port_sample") <- sample
      attr(rv, "tidymodules_is_parent") <- is_parent
      attr(rv, "tidymodules_inherit") <- inherit
      attr(rv, "tidymodules_module_ns") <- self$module_ns

      # Only add attributes to port at definition
      if (is.logical(p)) {
        attr(p, "tidymodules") <- TRUE
        attr(p, "tidymodules_port_slot") <- TRUE
        attr(p, "tidymodules_port_type") <- type
        attr(p, "tidymodules_port_id") <- private$countPort(type) + 1
        attr(p, "tidymodules_port_name") <- name
        attr(p, "tidymodules_port_description") <- description
        attr(p, "tidymodules_port_sample") <- sample
        attr(p, "tidymodules_is_parent") <- is_parent
        attr(p, "tidymodules_inherit") <- inherit
        attr(p, "tidymodules_module_ns") <- self$module_ns
      }
      rv[["port"]] <- p

      key <- ifelse(type == "input", "i", "o")
      existing_port <- self[[key]][[name]]
      if (!is.null(existing_port) && is.reactivevalues(existing_port)) {
        warning(paste0(
          "Skip adding ", type, " port '", name, "' to ", self$module_ns,
          " ! Port already defined ",
          ifelse(existing_port$parent,
            paste0("and inherited from parent ", self$parent_ns),
            "!"
          )
        ))
      } else {
        self[[key]][[name]] <- rv
        nv <- self$port_names[[type]]
        self$port_names[[type]] <- c(nv, name)
      }
    },
    updatePort = function(id = NULL, port = NULL, type = "input") {
      stopifnot((!is.null(id)))

      if (!is.null(port)) {
        if (is.reactivevalues(port)) {
          if (!is.null(attr(port, "tidymodules_operation")) &&
            attr(port, "tidymodules_operation") == "combine") {
            # We need to modify the combined reactive list here to remove the port sctructure
            isolate({
              p <- reactiveValues()
              for (k in names(port)) {
                p[[k]] <- port[[k]]$port
              }
              port <- p
            })
          } else if (!is.null(attr(port, "tidymodules_port_def")) &&
            attr(port, "tidymodules_port_def")) {
            port <- port$port
          } else {
            stop(paste0(deparse(substitute(port)), " is reactive list (reactiveValues). Provide a reactive function instead."))
          }
        } else if (!is.reactive(port)) {
          stop(paste0(deparse(substitute(port)), " is not reactive"))
        }

        key <- ifelse(type == "input", "i", "o")
        if (is.numeric(id)) {
          if (!id %in% seq(1, private$countPort(type))) {
            stop(paste0("Port Update Failure: Numeric ", type, " port [", id, "] not found in Module definition"))
          } else {
            id <- self$port_names[[type]][id]
          }
        }
        if (is.character(id) && !id %in% names(self[[key]])) {
          stop(paste0("Port Update Failure: Character ", type, " port [", id, "] not found in Module definition"))
        }

        # Attach module information to the output port
        # This will facilitate storage of module edges
        isolate({
          # Don't allow update of inherited port
          if (self[[key]][[id]][["parent"]]) {
            stop(paste0("Updating ", type, " port '", id, "' inherited from parent ", self$parent_ns, " is not permitted!"))
          }
          attrs <- attributes(self[[key]][[id]][["port"]])
          if (type == "output") {
            for (a in names(attrs)) {
              if (grepl("tidymodules", a)) {
                attr(port, a) <- attrs[[a]]
              }
            }
          }

          self[[key]][[id]][["port"]] <- port
        })
      }
    },
    updatePorts = function(ports = NULL, type = "input", is_parent = FALSE) {
      stopifnot(!is.null(ports))
      if (!is.reactivevalues(ports)) {
        stop(paste0(deparse(substitute(ports)), " is not a reactive list"))
      }

      key <- ifelse(type == "input", "i", "o")

      isolate({
        for (p in names(ports)) {
          if (p %in% self$port_names[[type]]) {
            stop(paste0("Adding port name ", p, " to module ", self$module_ns, " failed, it already exist in ", type, " port definition."))
          } else {
            port <- ports[[p]]
            rport <- port$port
            # couple of functions for managing inherited ports
            getParentReactive <- function(p) {
              parent_mod_ns <- attr(p, "tidymodules_module_ns")
              r <- reactive({
                force(p)
                force(self)
                force(parent_mod_ns)

                m <- self$parent_mod
                if (!is.null(parent_mod_ns)) {
                  m <- mod(parent_mod_ns)
                } else {
                  warning(paste0("Couldn't find parent module while passing input ports to ", self$module_ns))
                }

                if (type == "input") {
                  m$getInput(p$name)
                } else {
                  m$getOutput(p$name)
                }
              })

              # copy tm attributes
              attrs <- attributes(p$port)
              for (a in names(attrs)) {
                if (grepl("tidymodules", a)) {
                  attr(r, a) <- attrs[[a]]
                }
              }

              r
            }
            if (is_parent) {
              rport <- getParentReactive(port)
            }
            if (!(is_parent && !port$inherit)) {
              private$addPort(
                type = type,
                name = port$name,
                description = port$description,
                sample = port$sample,
                port = rport,
                is_parent = is_parent,
                inherit = port$inherit
              )
            }
          }
        }
      })
    },
    get = function(id = 1, type = "input") {
      data.port <- private$getPort(id, type)
      req(data.port)

      return(data.port[["port"]])
    },
    getPort = function(id = 1, type = "input") {
      key <- ifelse(type == "input", "i", "o")
      if (private$countPort(type) == 0) {
        warning(paste0("Module ", self$module_ns, " has no ", type, " ports"))
        return(NULL)
      }
      if (is.numeric(id)) {
        if (!id %in% seq(1, private$countPort(type))) {
          warning(paste0("Numeric ", type, " port [", id, "] not found in Module definition"))
          return(NULL)
        } else {
          id <- self$port_names[[type]][id]
        }
      }
      if (is.character(id) && !id %in% names(self[[key]])) {
        warning(paste0("Character ", type, " port [", id, "] not found in Module definition"))
        return(NULL)
      }
      return(self[[key]][[id]])
    },
    getPorts = function(type = "input") {
      key <- ifelse(type == "input", "i", "o")
      if (private$countPort(type) == 0) {
        return(NULL)
      }

      return(self[[key]])
    },
    printPorts = function(type = "input") {
      if (private$countPort(type) > 0) {
        for (p in 1:private$countPort(type)) {
          port <- private$getPort(p, type)
          pport <- NULL
          if (port$parent) {
            if (type == "input") {
              pport <- self$parent_mod$getInputPort(port$name)
            } else {
              pport <- self$parent_mod$getOutputPort(port$name)
            }
          }
          cat(
            paste0(
              "(", p, ") ",
              ifelse(
                port$parent,
                "(Inherit) ",
                ""
              ),
              port$name,
              " => ",
              ifelse(port$parent,
                ifelse(is.reactive(pport$port) || is.reactivevalues(pport$port), "OK", "Empty"),
                ifelse(is.reactive(port$port) || is.reactivevalues(port$port), "OK", "Empty")
              ),
              "\n"
            )
          )
        }
      }
    },
    trimParentNS = function() {
      if (!is.null(self$parent_ns)) {
        return(
          sub(
            paste0(self$parent_ns, "-"),
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
