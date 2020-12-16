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
#' Function for horizontal index from UFZ (inspired by TUM index for field data)
#' calculated from Lidar vertical profile data
#' @param prof Lidar profile as vector with height bins as names
#' @return Index of vertical forest structure
#' @keywords forest structure vertical heterogeneity Lidar profile
#' @export
#' @examples in progress

calc.lidar.vert.index.UFZ.from.prof <- function(prof){
  h.vec <- as.numeric(as.character(names(prof)))
  h.max <- max(h.vec)
  h.80 <- 0.8*h.max
  h.50 <- 0.5*h.max
  # Returns in layer 1
  ret1 <- sum(prof[h.vec >= h.80])
  # Returns in layer 2
  ret2 <- sum(prof[h.vec >= h.50 & h.vec < h.80])
  # Returns in layer 3
  ret3 <- sum(prof[h.vec < h.50 & h.vec > 0])
  # If max. H <= 5 m all trees belong to layer 1
  if(h.max <= 5){
    ret1 <- sum(prof[h.vec > 0])
    ret2 <- 0
    ret3 <- 0
  }
  # Calculate the Shannon index of the 3 layers, which corresponds to
  # a Lidar interpretation of the modified species profile index (Pretzsch, 2002),
  # where BA in each height class is replaced by return count in each height class.
  vec <- c(ret1, ret2, ret3)
  #vec <- c(1,2,3)
  vec <- vec[vec != 0]
  rel.vec <- vec/sum(vec, na.rm=T)
  (lidar.vert.index.TUM <- -sum(rel.vec*log(rel.vec), na.rm=T)/log(3))
  return(lidar.vert.index.TUM)
}
