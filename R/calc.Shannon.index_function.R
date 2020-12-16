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



#' Calculate the Shannon index of a profile
#'
#' Function that returns the Shannon diversity index of a vector
#' of counts or relative proportions (e.g. Lidar profile).
#' @param vec Vector of counts or relative proportions
#' @param ex.vec Vector indices of elements in vec that should be excluded
#' (e.g. to exclude ground return of a Lidar profile set to 1)
#' @return Shannon index
#' @keywords height profile Lidar Shannon index diversity heterogeneity
#' @export
#' @examples in progress

calc.ShannonIndex <- function(vec, ex.vec=NA){
  # Exclude certain elements
  if(!is.na(ex.vec)){
    vec <- vec[-ex.vec]
  }
  # Exclude classes with value 0
  no.zero.vec <- vec[vec != 0]
  # Calculate the relative contribution of each class
  rel.vec <- no.zero.vec/sum(no.zero.vec, na.rm=T)
  # Calculate the Shannon index of the vector and normalize with ln of the whole vector length
  (ShannonIndex <- -sum(rel.vec*log(rel.vec), na.rm=T)/log(length(vec)))
  return(ShannonIndex)
}
