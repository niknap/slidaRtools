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



#' Calculate gaussian density for a point in 2D space
#'
#' Function that returns for each X-Y-coordinate pair, the gaussian density around
#' a given center point and with a given width.
#' @param Xctr X-coordinate of the center point
#' @param Yctr Y-coordinate of the center point
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param w width of the Gauss distribution
#' @return Vector of density values for each input point
#' @keywords Gaussian distribution density weighting 2D
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

calc.2D.gaussian.density <- function(w, Xctr, Yctr, Xcor, Ycor){
  distance <- ((Xcor-Xctr)^2+(Ycor-Yctr)^2)^0.5
  norm.distance <- distance/w
  output <- exp(-5*norm.distance^2)
  return(output)
}
