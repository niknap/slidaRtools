#' Calculate Gaussian function value for a point
#'
#' Function that returns for each given point the value of a Gaussian function.
#' @param pt Coordinate of the point
#' @param amp Amplitude of the peak of the Gaussian curve
#' @param ctr Coordinate of the center point
#' @param sd Standard deviation of the Gaussian curve (for a curve that contains >99\% of the area between -1 and 1 use sd=0.4)
#' @return Vector of function values for each input point
#' @keywords Gaussian density weighting bell curve normal distribution
#' @export
#' @examples in progress

calc.gaussian.function <- function(pt, amp=1, ctr=0, sd=1){
  output <- amp*exp(-(pt-ctr)^2/(2*sd^2))
  return(output)
}
