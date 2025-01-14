#' colmat: Create a Color Matrix
#' @export
#' @examples
#' col.matrix<-colmat(nbreaks=10, xlab="My x label", ylab="My y label")
#'
#' # https://rfunctions.blogspot.com/2015/03/bivariate-maps-bivariatemap-function.html
#' @description Creates a color matrix to be used in bivariate.map() function.
#' @return Two outputs: a color matrix object to be used in bivariate.map() function, and a plot of the color matrix.
#' @param nbreaks numeric variable for number of breaks in color matrix
#' @param breakstyle character variable for style used compute class intervals in color matrix. This is uses classInt::classIntervals. Options are one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box".
#' @param upperleft upperleft color of color matrix
#' @param upperright upperright color of color matrix
#' @param bottomleft bottomleft color of color matrix
#' @param bottomright bottomright color of color matrix
#' @param xlab character variable
#' @param ylab character variable
#' @param plotLeg default TRUE, if FALSE, does not plot the color matrix when function ends
#' @param saveLeg default TRUE, if FALSE, does not save a ggplot file of the color matrix when function ends
colmat <- function(nbreaks = 3, breakstyle = "quantile",
                   upperleft = "#0096EB", upperright = "#820050",
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = TRUE) {
  library(tidyverse)
  require(ggplot2)
  require(classInt)
  if (breakstyle == "sd") {
    warning("SD breaks style cannot be used.\nWill not always return the correct number of breaks.\nSee classInt::classIntervals() for details.\nResetting to quantile",
            call. = FALSE, immediate. = FALSE)
    breakstyle <- "quantile"}
  # The colours can be changed by changing the HEX codes for:
  # upperleft, upperright, bottomleft, bottomright
  # From http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  # upperleft = "#64ACBE", upperright = "#574249", bottomleft = "#E8E8E8", bottomright = "#C85A5A",
  # upperleft = "#BE64AC", upperright = "#3B4994", bottomleft = "#E8E8E8", bottomright = "#5AC8C8",
  # upperleft = "#73AE80", upperright = "#2A5A5B", bottomleft = "#E8E8E8", bottomright = "#6C83B5",
  # upperleft = "#9972AF", upperright = "#804D36", bottomleft = "#E8E8E8", bottomright = "#C8B35A",
  # upperleft = "#DA8DC8", upperright = "#697AA2", bottomleft = "#E8E8E8", bottomright = "#73BCA0",
  # Similar to Teuling, Stockli, Seneviratnea (2011) [https://doi.org/10.1002/joc.2153]
  # upperleft = "#F7900A", upperright = "#993A65", bottomleft = "#44B360", bottomright = "#3A88B5",
  # Viridis style
  # upperleft = "#FEF287", upperright = "#21908D", bottomleft = "#E8F4F3", bottomright = "#9874A1",
  # Similar to Fjeldsa, Bowie, Rahbek 2012
  # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
  # Default from original source
  # upperleft = "#0096EB", upperright = "#820050", bottomleft= "#BEBEBE", bottomright = "#FFE60F",
  my.data <- seq(0, 1, .01)
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(my.data,
                                       n = nbreaks,
                                       style = breakstyle)
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- classInt::findColours(my.class, my.col)
  }
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>%
    mutate("Y" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>%
    pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>%
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(.$Y)) %>%
    dplyr::select(-c(4)) %>%
    mutate("Y" = rep(seq(from = 1, to = nbreaks, by = 1), each = nbreaks),
           "X" = rep(seq(from = 1, to = nbreaks, by = 1), times = nbreaks)) %>%
    mutate("UID" = row_number())
  # Use plotLeg if you want a preview of the legend
  if (plotLeg | saveLeg) {
    p <- ggplot(col.matrix.plot, aes((X*2)/10, (Y*2)/10, fill = HEXCode)) +
      geom_raster() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 1, by=0.2)) +
      scale_y_continuous(breaks = seq(0, 1, by=0.2))+
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 10, colour = "black",hjust = 0.5,
                                      vjust = 1),
            axis.text = element_text(size = 8),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
  }
  if (plotLeg) {
    print(p)
  }
  seqs <- seq(0, 100, (100 / nbreaks))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  attr(col.matrix, "breakstyle") <- breakstyle
  attr(col.matrix, "nbreaks") <- nbreaks
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    return(list(col.matrix = col.matrix,
                plot = p))
  } else {
    return(list(col.matrix = col.matrix,
                plot = NULL))
  }
}

