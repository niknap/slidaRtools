#' Calculate Gaussian function value for a point in 2D space
#'
#' Function that returns for each X-Y-coordinate pair the value of a Gaussian function.
#' @param Xpt X-coordinate of the point
#' @param Ypt Y-coordinate of the point
#' @param amp Amplitude of the peak of the Gaussian curve
#' @param Xctr X-coordinate of the center point
#' @param Yctr Y-coordinate of the center point
#' @param sd Standard deviation of the Gaussian curve (for a curve that contains >99\% of the area between -1 and 1 use sd=0.4)
#' @return Vector of function values for each input point
#' @keywords Gaussian density weighting 2D bell curve normal distribution
#' @export
#' @examples in progress

calc.gaussian.function.2D <- function(Xpt, Ypt, amp=1, Xctr=0, Yctr=0, sd=1){
  dist <- ((Xpt-Xctr)^2+(Ypt-Yctr)^2)^0.5
  output <- amp*exp(-(dist)^2/(2*sd^2))
  return(output)
}
