library(tidymodules)

check_and_load("ggplot2")
check_and_load("dplyr")
check_and_load("shinycssloaders")


source(system.file(package = "tidymodules","shiny/examples/2_linked_scatter/linked_scatter.R"))

lsObj1 <- LinkedScatter$new()
lsObj2 <- LinkedScatter$new()
lsObj3 <- LinkedScatter$new()
lsObj4 <- LinkedScatter$new()
store <- Store$new()

ui <- fixedPage(
  h2("tidymodules : linked scatter"),
  tabsetPanel(type = "tabs",
      tabPanel("App",
        br(),
        lsObj1$ui(),
        textOutput("summary1"),
        lsObj2$ui(),
        textOutput("summary2"),
        lsObj3$ui(),
        lsObj4$ui()
      ),
      tabPanel("Store",
         br(),
         fluidRow(
           column(12, store$ui())
         )
      )
  )
)


server <- function(input, output, session) {
  d<-reactive({
    mpg
  })
  
  o <- reactiveValues(
    left = c("cty", "hwy"),
    right = c("drv", "hwy")
  )
  
  # callModules()
  lsObj1$callModule()
  lsObj2$callModule()
  lsObj3$callModule()
  lsObj4$callModule()
  store$callModule()
  
  observe({
    o %>>2% lsObj1 %>>2% lsObj2 %>>2% lsObj3 %>>2%lsObj4
    d %>1% lsObj1 %2>1% lsObj2 %2>1% lsObj3 %2>1% lsObj4
  })
  
  output$summary1 <- renderText({
    df <- getMod(1)$getOutput(1)
    sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
  })
  
  output$summary2 <- renderText({
    df <- getMod(2)$getOutput(1)
    sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
  })
}

shinyApp(ui, server)