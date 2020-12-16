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



#' Check if point lies within a spheroid
#'
#' Function that checks for each X-Y-coordinate pair, whether the point lies in a
#' spheroid of a given center, radius and height. Works with single numbers or matrices of
#' X- and Y-coordinates as input.
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param Zcor Z-coordinate of the point
#' @param Xctr X-coordinate of the center
#' @param Yctr Y-coordinate of the center
#' @param Zctr Z-coordinate of the center
#' @param radius ...of the equator plain of the spheroid
#' @param height ...of the vertical axis of the spheroid
#' @return boolean or matrix of booleans
#' @keywords in spheroid shape
#' @export
#' @examples in progress

in.spheroid <- function(Xcor, Ycor, Zcor, Xctr, Yctr, Zctr, radius, height){
  result <- ifelse((Xcor - Xctr)^2 / (radius^2) +
                  (Ycor - Yctr)^2 / (radius^2) +
                  (Zcor - Zctr)^2 / ((height / 2)^2) - 1 <= 0, 1, 0)
  return(result)
}
