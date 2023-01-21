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



#' Check if point lies within a circle
#'
#' Function that checks for each X-Y-coordinate pair, whether the point lies in a
#' circle of a given center and radius. Works with single numbers or matrices of
#' X- and Y-coordinates as input.
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param Xctr X-coordinate of the circle center
#' @param Yctr Y-coordinate of the circle center
#' @param radius ...of the circle
#' @return boolean or matrix of booleans
#' @keywords in circle overlap overlay
#' @export
#' @examples in progress
#' @author Nikolai Knapp

in_circle <- function(Xcor, Ycor, Xctr, Yctr, radius){
  result <- ifelse((Xcor - Xctr)^2 + (Ycor - Yctr)^2 <= radius^2, T, F)
  return(result)
}

