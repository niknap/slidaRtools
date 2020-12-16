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



#' Calculate vertical structure index suggestion of Victor Cazcarra (DLR)
#' from Lidar profile or vertical foliage profile
#'
#' Function for vertical index from DLR calculated from Lidar
#' XYZ-table data (point cloud or CHM).
#' The equation for the index V is: V = 1 - (abs(CM - Hperc) / Hperc) with
#' CM being the center of mass (equal to MCH or TCH, depending if input is
#' point cloud or CHM-raster) and Hperc being a custom fraction of the max.
#' height (Hmax) in the plot (default 50 percent).
#' @param XYZtable Lidar point cloud or CHM in XYZ-table format
#' @param perc Fraction between 0 and 1 (default 0.5) to specify Hperc = perc * Hmax
#' @param h.min Minimum height below which Lidar returns are ignored
#' @return Index of vertical forest structure
#' @keywords forest structure vertical heterogeneity Lidar XYZ
#' @export
#' @examples in progress

calc.lidar.vert.index.DLR2.rel.centr.mass.from.prof <- function(prof, perc=0.5, h.min=1){
  h.vec <- as.numeric(as.character(names(prof)))
  prof <- prof[h.vec >= h.min]
  h.vec <- h.vec[h.vec >= h.min]
  # Check if the profile exceeds the minimum height threshold
  # otherwise return 0
  if(length(prof) > 0){
    h.max <- max(h.vec, na.rm=T)
    h.perc <- perc*h.max
    # Calculate center of mass (weighted mean height of the profile)
    CM <- sum(prof*h.vec, na.rm=T)/sum(prof, na.rm=T)
    # Calculate the vertical index
    V.index <- 1 - (abs(CM - h.perc) / h.perc)
    return(V.index)
  } else {
    return(0)
  }
}




