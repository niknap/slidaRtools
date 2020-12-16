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



#' Make a polygon
#'
#' Function that creates a spatial polygon data frame from given vertex coordinates.
#' @param vertex.mx Matrix with two columns (X- and Y-coordinates) where each row represents one vertex
#' @return SpatialPolygonsDataFrame
#' @keywords vector shape polygon vertex area
#' @export
#' @examples test.spdf <- make.polygon(vertex.mx=matrix(c(1,1,5,1,3,3), ncol=2, byrow=T))
#' plot(test.spdf)

make.polygon <- function(vertex.mx){
  require(sp)
  require(rgeos)
  # Add the first vertex also to the end, to create a closed polygon
  vertex.mx <- rbind(vertex.mx, vertex.mx[1,])
  # Make a spatial polygon
  p = Polygon(vertex.mx)
  ps = Polygons(list(p), 1)
  sps = SpatialPolygons(list(ps))
  df = data.frame(ID=1)
  spdf = SpatialPolygonsDataFrame(sps, df)
  return(spdf)
}


