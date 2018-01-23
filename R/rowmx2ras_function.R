#' Convert byrow-matrix to raster
#'
#' Convert a matrix with origin in upper left corner and with cell order from
#' left to right (byrow) to a raster with origin in lower left corner
#' @param mx Input matrix
#' @return Raster object
#' @keywords matrix raster conversion byrow transposition coordinate origin
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

rowmx2ras <- function(mx){
  #require(raster)
  mx <- mx[nrow(mx):1,]
  ras <- raster(mx, xmx=ncol(mx), ymx=nrow(mx))
  return(ras)
}