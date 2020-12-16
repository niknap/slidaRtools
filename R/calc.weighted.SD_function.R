# Copyright (C) 2017 Dr. Nikolai Knapp, UFZ
#
# This file is part of the slidaRtools R package.
#
# The slidaRtools R package is free software: you can redistribute
# it and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# slidaRtools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with slidaRtools If not, see <http://www.gnu.org/licenses/>.



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


