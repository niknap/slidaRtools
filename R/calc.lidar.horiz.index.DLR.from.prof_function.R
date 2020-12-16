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



#' Calculate horizontal structure index from Lidar profile
#'
#' Function for horizontal index from DLR calculated from Lidar
#' vertical profile data
#' @param prof Lidar profile as vector with height bins as names
#' @param global.h.max Maximal tree height in the region for normalization (Default=50)
#' @return Index of horizontal forest structure
#' @keywords forest structure horizontal heterogeneity Lidar profile
#' @export
#' @examples in progress

calc.lidar.horiz.index.DLR.from.prof <- function(prof, global.h.max=50){
  h.vec <- as.numeric(as.character(names(prof)))
  h.max <- max(h.vec)
  h.60 <- 0.6*h.max
  ret1 <- sum(prof[h.vec >= h.60])
  ret.all <- sum(prof)
  lidar.horiz.index.DLR <- (ret1/ret.all)*h.max/global.h.max
  return(lidar.horiz.index.DLR)
}












