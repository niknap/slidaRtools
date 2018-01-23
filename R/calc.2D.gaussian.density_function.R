#' Calculate gaussian density for a point in 2D space
#'
#' Function that returns for each X-Y-coordinate pair, the gaussian density around
#' a given center point and with a given width.
#' @param Xctr X-coordinate of the center point
#' @param Yctr Y-coordinate of the center point
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param w width of the Gauss distribution
#' @return Vector of density values for each input point
#' @keywords Gaussian distribution density weighting 2D
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

calc.2D.gaussian.density <- function(w, Xctr, Yctr, Xcor, Ycor){
  distance <- ((Xcor-Xctr)^2+(Ycor-Yctr)^2)^0.5
  norm.distance <- distance/w
  output <- exp(-5*norm.distance^2)
  return(output)
}
