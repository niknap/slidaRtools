#' Check if point lies within a cuboid
#'
#' Function that checks for each X-Y-coordinate pair, whether the point lies in a
#' cuboid of a given center, side length and height. Works with single numbers or matrices of
#' X- and Y-coordinates as input.
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param Zcor Z-coordinate of the point
#' @param Xctr X-coordinate of the center
#' @param Yctr Y-coordinate of the center
#' @param Zbase Z-coordinate of the lower end
#' @param sidelength horizontal length and width of the cuboid
#' @param height ...of the cuboid
#' @return boolean or matrix of booleans
#' @keywords in cuboid shape
#' @export
#' @examples in progress

# Function that determines which voxels belong to a cuboid tree crown
in.cuboid <- function(Xcor, Ycor, Zcor, Xctr, Yctr, Zbase, sidelength, height){
  result <- ifelse(Zcor >= Zbase & Zcor <= (Zbase + height) &
                          Ycor >= (Yctr - sidelength/2) & Yarray <= (Yct + sidelength/2) &
                          Xcor >= (Xctr - sidelength/2) & Xarray <= (Xct + sidelength/2), 1, 0)
  return(result)
}

