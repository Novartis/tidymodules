
Scatter3DPlot <- R6::R6Class(
  "Scatter3DPlot", 
  inherit = BasePlot,
  public = list(
    ui = function(header = NULL){
      super$ui(plotlyOutput, header)
    },
    renderPlot = function(d,cols){
      shiny::req(length(cols())>3)
      renderPlotly({
        d <- as.data.frame(d())
        cols <- cols()
        plot_ly(x = d[,cols[1]], y = d[,cols[2]], z = d[,cols[3]],color = d[,cols[4]]) %>%
          layout(scene = list(xaxis = list(title = cols[1]),
                              yaxis = list(title = cols[2]),
                              zaxis = list(title = cols[3])))
      })
    }
  )
)


