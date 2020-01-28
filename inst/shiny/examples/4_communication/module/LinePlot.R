
LinePlot <- R6::R6Class(
  "LinePlot", 
  inherit = BasePlot,
  public = list(
    chart = function(data,cols){
      return(reactive({
        aes <- self$aes(cols())
        ggplot(data = data(), aes) + 
          geom_point() + 
          geom_line()
      }))
    }
  )
)


