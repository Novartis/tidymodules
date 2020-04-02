
#' 
#' This TidyModule is used to explore the content of the ModStore.
#'
#' @description
#' Store is a TidyModule that can be used in your application to list existing applications, sessions and display your session's modules and edges.
#'
#' @details
#' Should be initialized and injected in your application.
#' 
#' @export
Store <- R6::R6Class(
  classname = "Store",
  inherit = TidyModule,
  public = list(
    #' @description
    #' Store's ui function.
    #' @return UI elements.
    ui = function(){
      tagList(
        tabsetPanel(
          id = "store_ID",
          type = "tabs",
          shiny::tabPanel("Sessions",
                   fluidRow(
                     br(),
                     DT::dataTableOutput(self$ns("sessions"))
                   )
          ),
          shiny::tabPanel("Mods",
                   fluidRow(
                     br(),
                     DT::dataTableOutput(self$ns("mods"))
                   )
          ),
          shiny::tabPanel("Edges",
                   fluidRow(
                     br(),
                     DT::dataTableOutput(self$ns("edges"))
                   )
          ),
          shiny::tabPanel("Port Mapping",
                   fluidRow(
                     br(),
                     visNetwork::visNetworkOutput(self$ns("portD"),width = "100%",height = "800px")
                   )
          )
        )
      )
    },
    #' @description
    #' Store's server function.
    #' @param input Shiny input.
    #' @param output Shiny output
    #' @param session Shiny session
    server = function(input, output, session){
      # Mandatory
      super$server(input,output,session)
      
      session_df <- reactive({
        s <- self$getStore()
        d <- data.frame(aid = NULL, path = NULL, sid = NULL, created = NULL, mod_cnt = NULL, edge_cnt = NULL)
        
        for(aid in names(s$getSessions())){
          for (sid in names(s$getSessions()[[aid]])) {
            ses <- s$getSessions()[[aid]][[sid]]
            mcount <- length(ses$collection)
            ecount <- nrow(ses$edges)
            d <- rbind(d,data.frame(
              aid = aid,
              path = ses$path,
              sid = sid,
              created = ses$created,
              updated = ses$updated,
              mod_cnt = mcount,
              edge_cnt = ecount))
          }
        }
        rownames(d) <- NULL
        
        d
      })
      
      mods_df <- reactive({
        s <- self$getStore()
        d <- do.call(
          rbind,
          lapply(
            s$getMods(self),
            function(l){
              data.frame(
                namespace = l$module_ns,
                class = paste(class(l),collapse = " <- "), 
                parent = ifelse(is.null(l$parent_ns),"",l$parent_ns), 
                created = l$created, 
                in_ports = l$countInputPort(),
                out_ports = l$countOutputPort())
            }
          )
        )
        
        rownames(d) <- 1:nrow(d)
        
        d
      })
      
      edges_df <- reactive({
        s <- self$getStore()
        e <- s$getEdges(self)
        req(nrow(e) != 0)
        
        e
      })
      
      output$sessions <- DT::renderDataTable({
        session_df()
      })
      
      output$edges <- DT::renderDataTable({
        edges_df()
      })
      
      output$mods <- DT::renderDataTable({
        d <- mods_df()
      })
        
      output$portD <- visNetwork::renderVisNetwork({
        
        edges <- edges_df()
        nodes <- mods_df()
        
        e <- edges %>% 
          mutate(
            font.size = 5,
            label = paste0(fport," ",mode,ifelse(is.na(comment),"",paste0("(",comment,")"))," ",tport)) %>% 
          select(from,to,label,font.size)
        
        # # minimal example
        # nodes <- data.frame(id = 1:3)
        # edges <- data.frame(from = c(1,2), to = c(1,3))
        
        nId <- c(as.vector(e$from),as.vector(e$to),as.vector(nodes$namespace)) %>% unique()
        nType <- rbind(
          data.frame(name=edges$from,class=edges$fclass),
          data.frame(name=edges$to,class=edges$tclass)) %>% unique()
        nClass <- as.character(nType[match(nId,nType$name),"class"])
        nShape <- ifelse(nClass == "TidyModule" | is.na(nClass), "square",
                         ifelse(nClass == "reactive","box","box"))
        nColor <- ifelse(nClass == "TidyModule" | is.na(nClass), "lightblue",
                         ifelse(nClass == "reactive","orange","grey"))
        
        nGroup <- ifelse(nClass == "TidyModule" | is.na(nClass), "A",
                         ifelse(nClass == "reactive","B","C"))
        
        visNetwork::visNetwork(
          data.frame(
            id = nId,
            label = nId,
            group = nGroup,
            shape = nShape,
            #color = nColor,
            shadow = TRUE,
            value = 10),
          e,
          height = "100%", 
          width =  "100%"
        ) %>% 
          visNetwork::visEdges(shadow = TRUE,
                   arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),
                   color = list(color = "lightblue", highlight = "yellow")) %>%
          #visHierarchicalLayout(direction = "RL", levelSeparation = 500)
          visNetwork::visLayout(randomSeed = 12)
      })
        
    }
  )
)


