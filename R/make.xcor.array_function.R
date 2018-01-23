#' Make a X-coordinate array
#'
#' Create a 3D array that contains the X-coordinate of each cell as cell value
#' @param minx minimal X-coordinate
#' @param maxx maximal X-coordinate
#' @param miny minimal Y-coordinate
#' @param maxy maximal Y-coordinate
#' @param minz minimal Z-coordinate
#' @param maxz maximal Z-coordinate
#' @param res side length of one cell
#' @return 3D array of X-coordinates
#' @keywords X coordinate array 3D voxel
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.xcor.array <- function(minx=0, maxx, miny=0, maxy, minz=0, maxz, res=1){
  nx <- ceiling((maxx-minx) / res)
  ny <- ceiling((maxy-miny) / res)
  nz <- ceiling((maxz-minz) / res)
  arr <- array(data=rep(1:nx, times=ny*nz), dim=c(nx, ny, nz))
  return(arr)
}
