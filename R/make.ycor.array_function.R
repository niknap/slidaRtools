#' Make a Y-coordinate array
#'
#' Create a 3D array that contains the Y-coordinate of each cell as cell value
#' @param minx minimal X-coordinate
#' @param maxx maximal X-coordinate
#' @param miny minimal Y-coordinate
#' @param maxy maximal Y-coordinate
#' @param minz minimal Z-coordinate
#' @param maxz maximal Z-coordinate
#' @param res side length of one cell
#' @return 3D array of Y-coordinates
#' @keywords Y coordinate array 3D voxel
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.ycor.array <- function(minx=0, maxx, miny=0, maxy, minz=0, maxz, res=1){
  nx <- ceiling((maxx-minx) / res)
  ny <- ceiling((maxy-miny) / res)
  nz <- ceiling((maxz-minz) / res)
  arr <- array(data=rep(1:ny, each=nx, times=nz), dim=c(nx, ny, nz))
  return(arr)
}
