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



#' Convert coordinates from decimal degrees to degrees, minutes and seconds
#'
#' Function that converts between coordinate notations
#' @param dd Decimal degrees
#' @return Degrees, minutes and seconds
#' @keywords decimal degree coordinate minute second
#' @export
#' @examples dec.deg.to.deg.min.sec(10.51)

dec.deg.to.deg.min.sec <- function(dd){
  d <- floor(dd)
  m <- floor((dd - d) * 60)
  s <- (dd - d - m/60) * 3600
  result <- c(d, m, s)
  names(result) <- c("deg", "min", "sec")
  return(result)
}
