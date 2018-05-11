#' Calculate weighted standard deviation
#'
#' Function for weighted SD calculation in case of already
#' aggregated data (e.g. Lidar profile)
#' @param val Vector of values
#' @param wt Vector of weights (e.g. counts or relative proportions)
#' @return Standard deviation
#' @keywords weighted SD standard deviation variance
#' @export
#' @examples val.vec <- c(3, 5, 4, 1)
#' wt.vec <- c(1, 1, 5, 5)
#' weighted.SD(val=val.vec, wt=wt.vec)

calc.weighted.SD <- function(val, wt){
  # Remove NAs
  val.na <- is.na(val)
  wt.na <- is.na(wt)
  vec.na <- val.na | wt.na
  val <- val[!vec.na]
  wt <- wt[!vec.na]
  # Check how many non-zero values the weights vector contains
  if(length(unique(wt[wt != 0]) > 1)){
    return((sum(wt*(val-sum(wt*val)/sum(wt))^2)/(((length(wt[wt>0])-1)*sum(wt))/length(wt[wt>0])))^0.5)
  }else{
    return(0)
  }
}


