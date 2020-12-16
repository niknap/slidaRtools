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



#' Calculate vertical structure index
#'
#' Function for vertical index from TUM calculated from inventory data.
#' Basal area of trees will be distributed based on tree's crown length
#' @param H.vec Vector of tree heights [m]
#' @param D.vec Vector of tree DBH [m]
#' @param CL.vec Vector of tree crown length (tree height - crown base height) [m]
#' @return Index of vertical forest structure
#' @keywords forest structure vertical heterogeneity
#' @export
#' @examples calc.field.vert.index.BA.layer.TUM(40,1,20)

calc.field.vert.index.BA.layer.TUM <- function(H.vec, D.vec, CL.vec){
  h.max <- max(H.vec, na.rm=T)
  h.80 <- 0.8*h.max
  h.50 <- 0.5*h.max

  # Basal area in layer 1 (100%-80%)
  BA1 = 0
  # Basal area in layer 2 (80%-50%)
  BA2 = 0
  # Basal area in layer 3 (50%-0%)
  BA3 = 0

  for (tree in 1:length(H.vec)) {
    tree.dbh = D.vec[tree]
    tree.ba = pi*(tree.dbh/2)^2
    tree.height = H.vec[tree]
    tree.crown_length = CL.vec[tree]
    tree.crown_base = H.vec[tree] - CL.vec[tree]
    if(tree.height >= h.80) {
      ba.fraction.80 = min((tree.height-h.80)/tree.crown_length,1)
      if(tree.crown_base >= h.50) {
        ba.fraction.50 = max((h.80-tree.crown_base)/tree.crown_length,0)
        ba.fraction.0 = 0
      }
      if(tree.crown_base < h.50) {
        ba.fraction.50 = (h.80-h.50)/tree.crown_length
        ba.fraction.0 = max((h.50-tree.crown_base)/tree.crown_length,0)
      }
    }
    if(tree.height < h.80 & tree.height >= h.50 ) {
      ba.fraction.80 = 0
      ba.fraction.50 = min((tree.height-h.50)/tree.crown_length,1)
      ba.fraction.0 = max((h.50-tree.crown_base)/tree.crown_length,0)
    }
    if(tree.height < h.50) {
      ba.fraction.80 = 0
      ba.fraction.50 = 0
      ba.fraction.0 = 1
    }

    BA1 = BA1 + (tree.ba * ba.fraction.80)
    BA2 = BA2 + (tree.ba * ba.fraction.50)
    BA3 = BA3 + (tree.ba * ba.fraction.0)
  }

  # If max. H <= 5 m all trees belong to layer 1
  if(h.max <= 5){
    BA1 <- sum(pi*(D.vec/2)^2)
    BA2 <- 0
    BA3 <- 0
  }
  # Calculate the Shannon index of the relative BA in 3 layers, which corresponds to
  # the modified species profile index (Pretzsch, 2002)..
  vec <- c(BA1, BA2, BA3)
  vec <- vec[vec != 0]
  rel.vec <- vec/sum(vec, na.rm=T)
  (field.vert.index.TUM <- -sum(rel.vec*log(rel.vec), na.rm=T)/log(3))
  return(field.vert.index.TUM)
}
