
ScatterPlot <- R6::R6Class(
  "ScatterPlot", 
  inherit = BasePlot,
  public = list(
    chart = function(data, cols) {
      return(reactive({
        aes <- self$aes(cols())
        ggplot(data(), aes) +
          geom_point(aes(color = selected_)) +
          scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
      }))
    }
    
  )
)


