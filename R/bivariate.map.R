#' bivariate.map: Create a Bivariate Map
#' @export
#' @examples
#' # https://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
#' @description Creates a Bivariate Map using two rasters and a color matrix created with colmat() function.
#' @return A plot with the bivariate map.
#' @param rasterx raster
#' @param rastery raster
#' @param colormatrix color matrix from colmat() function
#' @param nbreaks number of breaks in color matrix (same as used when using colmat() function)
bivariate.map <- function(rasterx, rastery, colormatrix = col.matrix) {

  require(terra)
  require(classInt)

  colormatrix <- colormatrix[[1]]

  quanx <- rasterx[]
  tempx <- data.frame(quanx, quantile = rep(NA, length(quanx)))
  brks <- with(tempx, classIntervals(quanx,
                                     n = attr(colormatrix, "nbreaks"),
                                     na.rm=TRUE,
                                     style = attr(colormatrix, "breakstyle"))$brks)
  ## Add (very) small amount of noise to all but the first break
  ## https://stackoverflow.com/a/19846365/1710632
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r1 <- within(tempx, quantile <- base::cut(quanx,
                                            breaks = brks,
                                            labels = 2:length(brks),
                                            include.lowest = TRUE))
  quantr <- data.frame(r1[, 2])
  quany <- rastery[]
  tempy <- data.frame(quany, quantile = rep(NA, length(quany)))
  brks <- with(tempy, classIntervals(quany,
                                      n = attr(colormatrix, "nbreaks"),
                                      na.rm=TRUE,
                                      style = attr(colormatrix, "breakstyle"))$brks)
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r2 <- within(tempy, quantile <- base::cut(quany,
                                            breaks = brks,
                                            labels = 2:length(brks),
                                            include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colormatrix
  cn <- unique(colormatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1, col.matrix2[i] <- which(
             col.matrix2[i] == cn
           )[1]
    )
  }
  v1 <- as.numeric.factor(quantr[, 1])
  v2 <- as.numeric.factor(quantr2[, 1])
  cols <- pbapply::pbsapply(1:length(v1), function(i) {
    as.numeric(col.matrix2[v2[i],v1[i]])
  })
  r <- rasterx
  r[] <- cols
  return(r)
}
