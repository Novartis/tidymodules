library(tidymodules)

check_and_load("RColorBrewer")

source(system.file(package = "tidymodules","shiny/examples/4_communication/module/Panel.R"))
source(system.file(package = "tidymodules","shiny/examples/3_nested_modules/Kmeans.R"))
source(system.file(package = "tidymodules","shiny/examples/3_nested_modules/ColorPicker.R"))

km_module <- Kmeans$new()

ui <- fixedPage(
  h2("tidymodules : Nested module example"),
  km_module$ui()
)

server <- function(input, output, session) {
  km <- km_module$callModule()
}

shinyApp(ui, server)
