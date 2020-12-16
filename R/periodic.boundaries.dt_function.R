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



#' Periodic boundaries
#'
#' Function that creates periodic boundaries for point clouds, i.e. points
#' that exceed the boundaries of the area re-appear on the opposite side
#' of the area.
#' @param xyz.dt Data.table containing X-, Y- and Z-coordinates of the points
#' @param minx Minimum X-coordinate of the area
#' @param maxx Maximum X-coordinate of the area
#' @param miny Minimum Y-coordinate of the area
#' @param maxy Maximum Y-coordinate of the area
#' @return data.table of X-, Y- and Z-coordinates with periodic boundaries
#' @keywords periodic boundaries border edge effects closed wrapped world tree crowns point cloud
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

periodic.boundaries.dt <- function(xyz.dt, minx, maxx, miny, maxy){
  require(data.table)
  xyz.dt <- data.table(xyz.dt)
  # Calculate the extent in both horizontal dimensions
  extx <- maxx - minx
  exty <- maxy - miny
  # Calculate the new X- and Y-values using modulo division
  xyz.dt[, X := (X - minx) %% extx + minx]
  xyz.dt[, Y := (Y - miny) %% exty + miny]
  return(xyz.dt)
}


