#' Check if point lies within a cylinder
#'
#' Function that checks for each X-Y-coordinate pair, whether the point lies in a
#' cylinder of a given center, radius and height. Works with single numbers or matrices of
#' X- and Y-coordinates as input.
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param Zcor Z-coordinate of the point
#' @param Xctr X-coordinate of the center
#' @param Yctr Y-coordinate of the center
#' @param Zbase Z-coordinate of the lower end
#' @param radius ...of the cylinder
#' @param height ...of the cylinder
#' @return boolean or matrix of booleans
#' @keywords in cylinder shape
#' @export
#' @examples in progress

in.cylinder <- function(Xcor, Ycor, Zcor, Xctr, Yctr, Zbase, radius, height){
  result <- ifelse(Zcor >= Zbase & Zcor <= (Zbase + height) &
                          (Xcor - Xctr)^2 + (Ycor - Yctr)^2 <= radius^2, 1, 0)
  return(result)
}

