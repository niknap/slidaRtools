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



#' Make a spatial index matrix
#'
#' Create a sampling plot matrix for a given area with a
#' specific resolution
#' @param minx minimal X-coordinate
#' @param maxx maximal X-coordinate
#' @param miny minimal Y-coordinate
#' @param maxy maximal Y-coordinate
#' @param res side length of one spatial subunit
#' @return matrix of the arrangement of spatial indices
#' @keywords spatial index plot number
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.spatial.index.matrix <- function(minx=0, maxx, miny=0, maxy, res){
  nx <- ceiling((maxx-minx) / res)
  ny <- ceiling((maxy-miny) / res)
  mx <- matrix(1:(nx*ny), nrow=ny, ncol=nx, byrow=T)
  return(mx)
}
