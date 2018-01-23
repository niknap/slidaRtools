#' Make a Y-coordinate matrix
#'
#' Create a matrix that contains the Y-coordinate of each cell as cell value
#' @param minx minimal X-coordinate
#' @param maxx maximal X-coordinate
#' @param miny minimal Y-coordinate
#' @param maxy maximal Y-coordinate
#' @param res side length of one cell
#' @return matrix of Y-coordinates
#' @keywords Y coordinate matrix
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.ycor.matrix <- function(minx=0, maxx, miny=0, maxy, res=1){
  nx <- ceiling((maxx-minx) / res)
  ny <- ceiling((maxy-miny) / res)
  mx <- matrix(rep(1:ny, each=nx), nrow=ny, ncol=nx, byrow=T)
  return(mx)
}

