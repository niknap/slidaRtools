#' Make a spatial index matrix
#'
#' Create a sampling plot matrix for a given area with a 
#' specific resolution
#' @param minx minimal X-coordinate
#' @param maxx maximal X-coordinate
#' @param miny minimal Y-coordinate
#' @param maxy maximal Y-coordinate
#' @param res side length of one spatial subunit
#' @return matrix of the arrangement of spatial indices
#' @keywords spatial index plot number
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.spatial.index.matrix <- function(minx=0, maxx, miny=0, maxy, res){
  nx <- ceiling((maxx-minx) / res)
  ny <- ceiling((maxy-miny) / res)
  mx <- matrix(1:(nx*ny), nrow=ny, ncol=nx, byrow=T)
  return(mx)
}
