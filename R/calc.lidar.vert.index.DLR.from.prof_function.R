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



#' Calculate vertical structure index from Lidar profile
#'
#' Function for vertical index from DLR calculated from Lidar
#' vertical profile data
#' @param prof Lidar profile as vector with height bins as names
#' @return Index of vertical forest structure
#' @keywords forest structure vertical heterogeneity Lidar profile
#' @export
#' @examples in progress

calc.lidar.vert.index.DLR.from.prof <- function(prof){
  h.vec <- as.numeric(as.character(names(prof)))
  h.max <- max(h.vec)
  h.50 <- round(0.5*h.max)
  # Returns in up layer
  up_layer <- sum(prof[h.vec >= h.50])
  # Returns in down layer
  down_layer <- sum(prof[h.vec < h.50 & h.vec >= 10])
  # Returns in big layer
  big_layer <- down_layer + up_layer
  # Coefficients
  coef_up <- up_layer / big_layer
  coef_down <- down_layer / big_layer
  # Calculate the vertical index according to the description by DLR.
  # Attention: does not work for empty layers, e.g. no returns between
  # 50% of max. H and 10 m.
  if(up_layer > 0 & down_layer > 0){
    lidar.vert.index.DLR <- - ((coef_up*log10(coef_up)) + (coef_down*log10(coef_down))) / log10(2)
  } else {
    lidar.vert.index.DLR <- 0
  }
  return(lidar.vert.index.DLR)
}
