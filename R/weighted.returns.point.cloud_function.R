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



#' Assign a weight to each point in a point cloud depending on total count points in voxel column
#'
#' Function that normalizes return weights in point clouds by dividing
#' the value 1 by the total count of returns in a voxel column of a given
#' horizontal resolution, e.g. if there are 4 returns in one vertical column
#' each gets a weight of 0.25
#' @param pc Data frame of the point cloud with first 3 columns representing X-, Y- and Z-coordinates
#' @param cell.size Horizontal side length of one spatial subunit (voxel column width in X- and Y-direction)
#' @return Data frame of the point cloud with additional column "ReturnWeight"
#' @keywords lidar point cloud xyz profile weighting normalization density correction
#' @export
#' @examples in progress

weighted.returns.point.cloud <- function(pc, cell.size=1){
  # Convert input to a data.frame
  pc <- as.data.frame(pc)
  # Make the sampling grid matrix
  minx <- min(pc$X)
  miny <- min(pc$Y)
  maxx <- max(pc$X)
  maxy <- max(pc$Y)
  maxx.gridcell <- ceiling((maxx-minx)/cell.size)
  maxy.gridcell <- ceiling((maxy-miny)/cell.size)
  grid.mx <- matrix(1:(maxx.gridcell*maxy.gridcell), nrow=maxy.gridcell, ncol=maxx.gridcell)
  # Calculate the cell number for each point
  pc$CellNo <- calcPlotNo(pc$X, pc$Y, cell.size, grid.mx)
  # Count how many returns fall into each cell
  returns.per.cell.count <- table(pc$CellNo)
  # Calculate the return weight by dividing 1 by the number of returns in that cell
  pc$ReturnWeight <- round(1/returns.per.cell.count[match(pc$CellNo, names(returns.per.cell.count))], 4)
  pc.out <- pc
  # Remove the cell number column
  pc.out <- subset(pc.out, select=-CellNo)
  return(pc.out)
}
