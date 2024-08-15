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



#' Convert raster to XYZ-data.table
#'
#' Convert a raster to a XYZ-data.table with Z being the pixel values
#' @param ras Input raster object
#' @return data.table
#' @keywords raster conversion coordinate XYZ table data.table
#' @export
#' @examples in progress
#' @author Nikolai Knapp

ras2xyzdt <- function(ras){
  require(raster)
  require(data.table)
  # Extract the resolution of the raster in X- and Y-direction
  resx <- res(ras)[1]
  resy <- res(ras)[2]
  # Extract the coordinates of the lower left corner of the raster
  minx <- xmin(ras)
  miny <- ymin(ras)
  # Convert the raster to a matrix
  mx <- raster::as.matrix(ras)
  # Transpose and resort the matrix to account for the different
  # coordinate conventions of rasters and matrices
  mx <- t(mx[nrow(mx):1, ])
  # Melt the matrix to a data.frame and assign column names
  xyz <- data.table::melt(mx)
  names(xyz) <- c("X", "Y", "Z")
  # Multiply the matrix coordinates with the raster resolution
  # and add the values of the lower left raster corner
  # to get the original coordinate values back
  xyz$X <- xyz$X * resx + minx
  xyz$Y <- xyz$Y * resy + miny
  return(xyz)
}












