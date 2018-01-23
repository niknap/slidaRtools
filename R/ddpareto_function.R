#' Calculate values of a discrete power law distribution
#'
#' Function that returns values of a discrete Pareto (power law)
#' distribution density (e.g. canopy gap sizes, tree diameters)
#' for a given scaling parameter lambda;
#' Code adapted from Asner et al. (2013) supplements.
#' @param x Value of the independent variable
#' @param lambda Scaling parameter
#' @return Value of the dependent variable
#' @keywords size distribution lambda discrete Pareto power law
#' @export
#' @examples in progress

ddpareto <- function(x, lambda){
  #require(VGAM)
  x^-lambda/zeta(lambda)
}
