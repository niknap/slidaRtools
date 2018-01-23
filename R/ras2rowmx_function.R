#' Convert raster to byrow-matrix
#'
#' Convert a raster with origin in lower left corner to a matrix with origin 
#' in upper left corner and with cell order filling row after row 
#' from left to right (byrow)
#' @param ras Input raster object
#' @return Matrix (cell order byrow)
#' @keywords matrix raster conversion byrow transposition coordinate origin
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

ras2rowmx <- function(ras){
  #require(raster)
  mx <- raster::as.matrix(ras)
  mx <- mx[nrow(mx):1, ]
  return(mx)
}