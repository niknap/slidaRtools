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



#' Extract point values from raster
#'
#' Function that extracts values from an underlying raster at the position
#' of each point of an overlying point cloud.
#' @param pc Input point cloud
#' @param ras Input raster
#' @return Vector of raster values for each point, that can be added as a
#' new column to the point cloud dataframe
#' @keywords raster point cloud extract CHM DTM
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

extract.point.values.from.raster <- function(pc, ras){
  # Convert input point cloud to a dataframe
  pc <- data.frame(pc)
  # Make the sampling grid matrix
  minx <- xmin(ras)
  miny <- ymin(ras)
  maxx <- xmax(ras)
  maxy <- ymax(ras)
  cell.size <- res(ras)[1]
  maxx.gridcell <- ceiling((maxx-minx)/cell.size)
  maxy.gridcell <- ceiling((maxy-miny)/cell.size)
  # Calculate the cell number for each point
  pc$CellNo <- calc.spatial.index(pc$X-minx+1, pc$Y-miny+1, cell.size)
  # Melt the raster to a dataframe
  ras.mx <- raster::as.matrix(ras)
  ras.mx <- t(ras.mx[nrow(ras.mx):1, ])
  ras.df <- melt(ras.mx)
  # Calculate the cell number for each raster cell
  ras.df$CellNo <- calc.spatial.index((cell.size*ras.df[,1]-cell.size), (cell.size*ras.df[,2]-cell.size), cell.size)
  # Match the point cloud points with the raster cells based on cell number and
  # add the corresponding raster values as a new column to the point cloud
  pc$RasterValue <- ras.df$value[match(pc$CellNo, ras.df$CellNo)]
  # Remove the cell number column
  pc <- subset(pc, select=-CellNo)
  RasterValue <- pc$RasterValue
  return(RasterValue)
}




