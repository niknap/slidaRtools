#' Calculate weighted mean
#'
#' Function for weighted mean calculation in case of already
#' aggregated data (e.g. Lidar profile)
#' @param val Vector of values
#' @param wt Vector of weights (e.g. counts or relative proportions)
#' @return Mean
#' @keywords weighted mean standard deviation variance
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

calc.weighted.mean <- function(val, wt){
  return(sum(wt*val)/sum(wt))
}

