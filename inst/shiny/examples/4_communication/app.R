library(tidymodules)

check_and_load("shinyWidgets")
check_and_load("ggplot2")
check_and_load("plotly")
check_and_load("DT")


# Load modules from ./module folder
app_dir <- system.file(package = "tidymodules","shiny/examples/4_communication")

sapply(
  list.files(file.path(app_dir,"module"),include.dirs = F, pattern = ".R",ignore.case = T), 
  function(f){ cat(paste0("Sourcing file :",f,"\n"));source(file.path(app_dir,"module",f)) }
)

Store$new()
Panel$new()
DatasetSelector$new("Marzie")
DataFilter$new("Stefan")
ColSelector$new("Renan")
PlotGenerator$new("Doug")

ui <- shiny::fluidPage(
  h2("tidymodules : Communication is the key to success!"),
  tags$br(),
  tabsetPanel(type = "tabs",
    tabPanel("App",
             br(),
             fluidRow(
               column(12, mod("Marzie")$ui())
             ),
             fluidRow(
               column(4, mod("Renan")$ui()),
               column(8, mod("Stefan")$ui())
             ),
             fluidRow(
               column(12,mod("Doug")$ui())
             )
    ),
    tabPanel("Help",
      tabsetPanel(type = "tabs",
        tabPanel("ModStore",
                 br(),
                 fluidRow(
                   column(12, 
                          mod(2)$ui(
                            status = "warning",
                            mod(1)$ui())
                          )
                 )
        ),
        tabPanel("ERD", img(src='ERD.svg', align = "center")),
        tabPanel("Ports", img(src='ports.svg', align = "center"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Add modules server logic
  callModules() 
  # Configure modules communication by connecting ports
  observe({
    # dataset selector provides data to
    # column mapper and row filter modules
    # getMod("Marzie") %1>1% getMod("Renan")
    browser()
    oport("Marzie","dataset") %->>% 
      iport("Renan","data") %->%
      iport("Stefan","data")
    
    mod("Marzie") %1>1% mod("Stefan")
    # the mappings are then used by the plot generator
    mod("Renan") %1>1% mod("Doug")
    # plot generator also takes raw and filtered data as input
    # by combining the two output ports in a named reactive list 
    combine_ports(
      raw    = oport("Marzie","dataset"),   # output 1 of data selector module
      filter = oport("Stefan","filtered")    # output 2 of data filter module
    ) %->% iport("Doug","tables")
  })
}


shinyApp(ui, server)
