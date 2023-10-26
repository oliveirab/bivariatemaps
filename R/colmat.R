#' colmat: Create a Color Matrix
#' @export
#' @examples
#' col.matrix<-colmat(nquantiles=10, xlab="My x label", ylab="My y label")
#'
#' # https://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
#' @description Creates a color matrix to be used in bivariate.map() function.
#' @return Two outputs: a color matrix object to be used in bivariate.map() function, and a plot of the color matrix.
#' @param nquantiles numeric variable for number of quantiles in color matrix
#' @param upperleft upperleft color of color matrix
#' @param upperright upperright color of color matrix
#' @param bottomleft bottomleft color of color matrix
#' @param bottomright bottomright color of color matrix
#' @param xlab character variable
#' @param ylab character variable
colmat<-function(nquantiles=10, upperleft="blue", upperright="red", bottomleft="grey", bottomright="yellow", xlab="x label", ylab="y label"){
  my.data<-seq(0,1,.01)
  my.class<-classInt::classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-classInt::findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-classInt::findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-classInt::findColours(my.class,my.col)}
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  col.matrix<-col.matrix[c(seqs), c(seqs)]
  return(col.matrix)
}
