#' Check if point lies within an "ice cone" like shape
#'
#' Function that checks for each X-Y-coordinate pair, whether the point lies in an
#' "ice cone" of a given center, radius and height. The ice cone consists of a hemi-
#' spheroid forming the upper half (ice ball) and a bottom-up cone forming the 
#' lower half (waffle).
#' Works with single numbers or matrices of X- and Y-coordinates as input.
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param Zcor Z-coordinate of the point
#' @param Xctr X-coordinate of the center
#' @param Yctr Y-coordinate of the center
#' @param Zctr Z-coordinate of the center
#' @param radius ...of the structure
#' @param height ...of the structure
#' @return boolean or matrix of booleans
#' @keywords in ice cone shape spheroid tree crown
#' @export
#' @examples in progress

in.icecone <- function(Xcor, Ycor, Zcor, Xctr, Yctr, Zctr, radius, height){
  # Calculate the radius at each height for the cone that forms the lower half
  RadiusAtHeight <- (radius / (0.5*height)) * (Zcor - (Zctr-0.5*height)) 
  # Check if the point is in lower half and in cone
  result <- ifelse(Zcor >= (Zctr-0.5*height) & Zcor < Zctr &
                   (Xcor - Xctr)^2 + (Ycor - Yctr)^2 <= RadiusAtHeight^2, 1, 
                   # Check if the point is in upper half and in hemi-spheriod
                   ifelse((Zcor >= Zctr) & (Xcor - Xctr)^2 / (radius^2) +
                          (Ycor - Yctr)^2 / (radius^2) +
                          (Zcor - Zctr)^2 / ((height / 2)^2) - 1 <= 0, 1, 0)
                   )
  return(result)
}



