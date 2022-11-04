Panel <- R6::R6Class(
  "Panel",
  inherit = tidymodules::TidyModule,
  public = list(
    ui = function(..., status = "default", header = NULL) {
      shinyWidgets::panel(
        id = self$module_ns,
        heading = tagList(
          shiny::tags$h3(
            class = "panel-title",
            tagList(
              paste0("#", self$module_ns, " - ", class(self)[1]),
              header
            )
          )
        ),
        status = status,
        tagList(...)
      )
    }
  )
)
