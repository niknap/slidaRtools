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

# H.vec <- c(10, 15, 25, 17)
# D.vec <- c(5, 22, 50, 20)
# CL.vec <- c(5, 8, 10, 7)

calc.field.vert.index.BA.layer.UFZ <- function(H.vec, D.vec, CL.vec){
  h.max <- max(H.vec, na.rm=T)
  hbin.vec <- 1:ceiling(h.max)
  vert.ba.vec <- rep(0, times=length(hbin.vec))
  names(vert.ba.vec) <- hbin.vec

  for (tree in 1:length(H.vec)) {
    #tree=1
    tree.dbh = D.vec[tree]
    tree.ba = pi*(tree.dbh/2)^2
    tree.height = ceiling(H.vec[tree])
    tree.crown_length = ceiling(CL.vec[tree])
    tree.crown_base = H.vec[tree] - CL.vec[tree]

    vert.ba.vec[tree.crown_base:tree.height] <- vert.ba.vec[tree.crown_base:tree.height]+tree.ba
  }

  #plot(names(vert.ba.vec)~vert.ba.vec, type="l")

  # If max. H <= 5 m all trees belong to layer 1
  if(h.max <= 5){
    vert.ba.vec <- 1
  }
  # Calculate the Shannon index of the relative BA in 3 layers, which corresponds to
  # the modified species profile index (Pretzsch, 2002)..
  vert.ba.vec <- vert.ba.vec[vert.ba.vec != 0]
  rel.vec <- vert.ba.vec/sum(vert.ba.vec, na.rm=T)
  (field.vert.index.TUM <- -sum(rel.vec*log(rel.vec), na.rm=T)/log(3))
  return(field.vert.index.TUM)
}


