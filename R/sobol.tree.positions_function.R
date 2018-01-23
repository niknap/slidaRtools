#' Distribute trees evenly in the plot considering their sizes
#'
#' Function that distributes trees evenly (quasi randomly) in the plot starting
#' with the largest and positioning them one after another with decreasing size
#' using the Sobol algorithm. The sobol algorithm needs initialization before
#' the first application of the function (see example below).
#' @param treesize.vec Vector of tree sizes (e.g. DBH, BA or height)
#' @param plotsize Sidelength of a square shaped plot [m]
#' @return Data.frame with 3 columns containing X- and Y-coordinates and tree sizes
#' @keywords tree stem position distribution pseudo quasi random Sobol coordinates space points
#' @export
#' @examples sobol.init <- sobol(1, dim=2, init=T)
#' pos <- sobol.tree.positions(c(3, 2, 1), plotsize=10)
#' plot(pos$Y ~ pos$X)
#' @author Nikolai Knapp

sobol.tree.positions <- function(treesize.vec, plotsize=20){
  #require(randtoolbox)
  # Sort the trees for decreasing size
  sort.treesize.vec <- sort(treesize.vec, decreasing=T)
  # Create an order vector to remember the original order
  order.vec <- order(treesize.vec, decreasing=T)
  # Run sobol with init=F to generate quasi-random tree coordinates with
  # even distributions of similarly sized trees
  coord.mx <- sobol(n=length(sort.treesize.vec), dim=2, init=F)
  # Transform the data and multiply the coordinates with plotsize to get them in meters
  coord.df <- as.data.frame(coord.mx)
  coord.df <- cbind(coord.df, sort.treesize.vec)
  names(coord.df) <- c("X", "Y", "size")
  coord.df$X <- round(coord.df$X * plotsize, 2)
  coord.df$Y <- round(coord.df$Y * plotsize, 2)
  # Sort back to original tree order before return
  coord.df <- coord.df[match(1:nrow(coord.df), order.vec), ]
  return(coord.df)
}



