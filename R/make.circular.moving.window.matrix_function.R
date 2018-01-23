#' Create a circular moving window matrix
#'
#' Function that creates a circular moving window matrix.
#' @param rad Radius of the considered (circular) neighborhood
#' @return matrix with 1 in circle and NA outside the circle
#' @keywords raster focal moving window filter circle
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.circular.moving.window.matrix <- function(rad=10){
  xdim <- 2*rad+1
  ydim <- 2*rad+1
  mx <- matrix(NA, nrow=xdim, ncol=ydim)
  in.circle <- function(Xarray, Yarray, Xct, Yct, radius){
    resultarray <- ifelse((Xarray - Xct)^2 + (Yarray - Yct)^2 <= radius^2, 1, NA)
    return(resultarray)
  }
  xmx <- matrix(rep(1:xdim, each=ydim), nrow=xdim, ncol=ydim)
  ymx <- matrix(rep(1:xdim, times=ydim), nrow=xdim, ncol=ydim)
  mx <- in.circle(xmx, ymx, rad+1, rad+1, rad)
  return(mx)
}
