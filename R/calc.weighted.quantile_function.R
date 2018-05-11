#' Calculate weighted quantile
#'
#' Function for weighted quantile calculation in case of already
#' aggregated data (e.g. Lidar profile)
#' @param val Vector of values
#' @param wt Vector of weights (e.g. counts or relative proportions)
#' @param quant Quantile as float between 0 and 1
#' @return Quantile
#' @keywords weighted quantile percentile
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

calc.weighted.quantile <- function(val, wt, quant){
  # Remove NAs
  val.na <- is.na(val)
  wt.na <- is.na(wt)
  vec.na <- val.na | wt.na
  val <- val[!vec.na]
  wt <- wt[!vec.na]
  # Calculate the proportion of the weights-vector-sum that
  # corresponds to the desired quantile
  target.wt <- quant*sum(wt)
  # Make the cumulative sum of the weights-vector
  cum.wt <- cumsum(wt)
  # The quantile is the value for which the cumulative sum
  # of weights equals or exceeds the target proportion for
  # the first time
  target.val <- val[min(which(cum.wt >= target.wt))]
  return(target.val)
}













