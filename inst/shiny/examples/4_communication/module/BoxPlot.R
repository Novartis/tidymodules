
BoxPlot <- R6::R6Class(
  "BoxPlot",
  inherit = BasePlot,
  public = list(
    chart = function(data, cols) {
      return(reactive({
        aes <- self$aes(cols())
        ggplot(data(), aes) +
          geom_boxplot()
      }))
    }
  )
)
