#' Convert raster to bycol-matrix
#'
#' Convert a raster with origin in lower left corner to a matrix with origin 
#' in upper left corner and with cell order filling column after column 
#' from top to bottom (bycol)
#' @param ras Input raster object
#' @return Matrix (cell order bycol)
#' @keywords matrix raster conversion bycol transposition coordinate origin
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

ras2colmx <- function(ras){
  #require(raster)
  mx <- raster::as.matrix(ras)
  mx <- t(mx[nrow(mx):1, ])
  return(mx)
}