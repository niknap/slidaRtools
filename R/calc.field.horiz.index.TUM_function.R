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



#' Calculate horizontal structure index
#'
#' # Function for horizontal index from TUM calculated from inventory data.
#' @param H.vec Vector of tree heights in m
#' @param D.vec Vector of tree DBH in cm
#' @param area ...of the plot in m2
#' @param max.SDI Maximum possible stand density index (SDI) (Default=700)
#' @return Index of horizontal forest structure
#' @keywords forest structure horizontal heterogeneity
#' @export
#' @examples in progress

calc.field.horiz.index.TUM <- function(H.vec, D.vec, area, max.SDI=700){
  h.max <- max(H.vec, na.rm=T)
  h.75 <- 0.75*h.max
  # Subset D of tall trees only
  d.layer1 <- D.vec[H.vec >= h.75]
  # Calculate N of tall trees per ha
  N <- length(d.layer1)*10000/area
  # Calculate quadratic mean diameter dg
  dg <- (mean(d.layer1^2))^0.5
  # Calculate stand density index
  SDI <- N*(25/dg)^-1.605
  field.horiz.index.TUM <- max(1-(SDI/max.SDI), 0)
  return(field.horiz.index.TUM)
}
