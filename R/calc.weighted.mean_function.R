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
  # Remove NAs
  val.na <- is.na(val)
  wt.na <- is.na(wt)
  vec.na <- val.na | wt.na
  val <- val[!vec.na]
  wt <- wt[!vec.na]
  return(sum(wt*val)/sum(wt))
}

