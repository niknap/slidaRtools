#' Calculate Skewness
#'
#' Function for calculation of skewness of an empirical distribution
#' @param vec Vector of data
#' @return Skewness (negative = left-skewed / positive = right-skewed)
#' @keywords skewness distribution shape profile mean symmetry
#' @export
#' @examples calc.skewness(c(1,2,3,3,3,4))
#' @author Nikolai Knapp

calc.skewness <- function(vec){
  vec.mean <- mean(vec, na.rm=T)
  vec.sd <- sd(vec, na.rm=T)
  return(sum(((vec-vec.mean)/vec.sd)^3)/length(vec))
}