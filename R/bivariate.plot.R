bivariate.plot <- function(bivmap,col.matrix) {
  plot(bivmap,
       frame.plot=F, axes=F, box=F, legend=F,
       col=as.vector(col.matrix))

  plot.new()
  par(mex = .6, plt = c(.12, .63, 0.1, 0.4), new = TRUE)
  col.matrix
}
