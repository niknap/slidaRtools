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
#' Function for vertical index from TUM calculated from inventory data
#' @param H.vec Vector of tree heights in m
#' @param D.vec Vector of tree DBH in cm
#' @return Index of vertical forest structure
#' @keywords forest structure vertical heterogeneity
#' @export
#' @examples in progress

calc.field.vert.index.TUM <- function(H.vec, D.vec){
  h.max <- max(H.vec, na.rm=T)
  h.80 <- 0.8*h.max
  h.50 <- 0.5*h.max
  # Basal area in layer 1
  BA1 <- sum(pi*((D.vec[H.vec >= h.80])/2)^2)
  # Basal area in layer 2
  BA2 <- sum(pi*((D.vec[H.vec < h.80 & H.vec >= h.50])/2)^2)
  # Basal area in layer 2
  BA3 <- sum(pi*((D.vec[H.vec < h.50])/2)^2)
  # If max. H <= 5 m all trees belong to layer 1
  if(h.max <= 5){
    BA1 <- sum(pi*(D.vec/2)^2)
    BA2 <- 0
    BA3 <- 0
  }
  # Calculate the Shannon index of the relative BA in 3 layers, which corresponds to
  # the modified species profile index (Pretzsch, 2002)..
  vec <- c(BA1, BA2, BA3)
  #vec <- c(1,2,3)
  vec <- vec[vec != 0]
  rel.vec <- vec/sum(vec, na.rm=T)
  (field.vert.index.TUM <- -sum(rel.vec*log(rel.vec), na.rm=T)/log(3))
  return(field.vert.index.TUM)
}
