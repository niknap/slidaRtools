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



#' Convert raster to byrow-matrix
#'
#' Convert a raster with origin in lower left corner to a matrix with origin
#' in upper left corner and with cell order filling row after row
#' from left to right (byrow)
#' @param ras Input raster object
#' @return Matrix (cell order byrow)
#' @keywords matrix raster conversion byrow transposition coordinate origin
#' @export
#' @examples in progress
#' @author Nikolai Knapp

ras2rowmx <- function(ras){
  #require(raster)
  mx <- raster::as.matrix(ras)
  mx <- mx[nrow(mx):1, ]
  return(mx)
}
