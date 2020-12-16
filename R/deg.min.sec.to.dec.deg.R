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



#' Convert coordinates from degrees, minutes and seconds to decimal degrees
#'
#' Function that converts between coordinate notations
#' @param deg Degrees
#' @param min Minutes
#' @param sec Seconds
#' @return Decimal degrees
#' @keywords decimal degree coordinate minute second
#' @export
#' @examples deg.min.sec.to.dec.deg(5, 4, 3)

deg.min.sec.to.dec.deg <- function(deg, min, sec){
  return(deg+(60*min+sec)/3600)
}
