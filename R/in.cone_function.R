#' Check if point lies within a cone
#'
#' Function that checks for each X-Y-coordinate pair, whether the point lies in a
#' cone of a given center, radius and height. Works with single numbers or matrices of
#' X- and Y-coordinates as input.
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param Zcor Z-coordinate of the point
#' @param Xctr X-coordinate of the center
#' @param Yctr Y-coordinate of the center
#' @param Zbase Z-coordinate of the lower end
#' @param radius ...of the ground plain of the cone
#' @param height ...of the cone
#' @return boolean or matrix of booleans
#' @keywords in cone shape
#' @export
#' @examples in progress

in.cone <- function(Xcor, Ycor, Zcor, Xctr, Yctr, Zbase, radius, height){
  RadiusAtHeight <- (- radius / height) * (Zcor - Zbase) + radius
  result <- ifelse(Zcor >= Zbase & Zcor <= (Zbase + height) &
                          (Xcor - Xctr)^2 + (Ycor - Yctr)^2 <= RadiusAtHeight^2, 1, 0)
  return(result)
}

