library(tidymodules)

check_and_load("RColorBrewer")

source(system.file(package = "tidymodules", "shiny/examples/4_communication/module/Panel.R"))
source(system.file(package = "tidymodules", "shiny/examples/3_nested_modules/Kmeans.R"))
source(system.file(package = "tidymodules", "shiny/examples/3_nested_modules/ColorPicker.R"))

km_module <- Kmeans$new()
store <- Store$new()

ui <- fixedPage(
  h2("tidymodules : Nested module example"),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "App",
      br(),
      km_module$ui()
    ),
    tabPanel(
      "Store",
      br(),
      fluidRow(
        column(12, store$ui())
      )
    )
  )
)

server <- function(input, output, session) {
  store$callModule()
  km_module$callModule()
}

shinyApp(ui, server)
