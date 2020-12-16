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



#' Check if point lies within a cuboid
#'
#' Function that checks for each X-Y-coordinate pair, whether the point lies in a
#' cuboid of a given center, side length and height. Works with single numbers or matrices of
#' X- and Y-coordinates as input.
#' @param Xcor X-coordinate of the point
#' @param Ycor Y-coordinate of the point
#' @param Zcor Z-coordinate of the point
#' @param Xctr X-coordinate of the center
#' @param Yctr Y-coordinate of the center
#' @param Zbase Z-coordinate of the lower end
#' @param sidelength horizontal length and width of the cuboid
#' @param height ...of the cuboid
#' @return boolean or matrix of booleans
#' @keywords in cuboid shape
#' @export
#' @examples in progress

# Function that determines which voxels belong to a cuboid tree crown
in.cuboid <- function(Xcor, Ycor, Zcor, Xctr, Yctr, Zbase, sidelength, height){
  result <- ifelse(Zcor >= Zbase & Zcor <= (Zbase + height) &
                          Ycor >= (Yctr - sidelength/2) & Yarray <= (Yct + sidelength/2) &
                          Xcor >= (Xctr - sidelength/2) & Xarray <= (Xct + sidelength/2), 1, 0)
  return(result)
}

