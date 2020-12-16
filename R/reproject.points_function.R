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



#' Reproject points in space from one coordinate reference system (CRS) to another
#'
#' Function that reprojects X- and Y-coordinates between different CRS.
#' @param xcor Vector of X-coordinates
#' @param ycor Vector of Y-coordinates
#' @param in.crs Original CRS of the data
#' @param out.crs Target CRS to which the points should be projected
#' @return SpatialPolygonsDataFrame
#' @keywords points coordinate reference system CRS projection geolocation
#' @export
#' @examples latlon <- CRS("+init=epsg:4326")
#' utm33n <- CRS("+init=epsg:32633")
#' reproject.points(xcor=c(0, 100), ycor=c(0, 100), in.crs=utm33n, out.crs=latlon)

reproject.points <- function(xcor, ycor, in.crs, out.crs){
  require(sp)
  coords <- cbind(xcor, ycor)
  # Make spatial data frame
  spdf <- SpatialPointsDataFrame(coords, data=data.frame(coords))
  # Assign the input CRS
  proj4string(spdf) <- in.crs
  # Project to the output CRS
  spdf <- spTransform(spdf, out.crs)
  return(spdf@coords)
}




