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



#' Point cloud thinning
#'
#' Function that thins a point cloud to a desired homogenous
#' horizontal point density by keeping a random subset of points
#' of a desired size in each spatial cell and discarding all other points.
#' @param pc Point cloud in XYZ data.table format (important: column names need to be X, Y and Z)
#' @param cell.size Side length of a spatial unit
#' @param returns.per.cell Number of returns that should be kept in each cell
#' @return data.table of the thinned point cloud in XYZ-format
#' @keywords thinning subset random point cloud density subsample
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

thin.point.cloud.dt <- function(pc, cell.size=1, returns.per.cell=4){
  require(data.table)
  # Convert input to a data.table
  pc <- data.table(pc)
  # Calculate the cell number for each point
  pc[, CellNo := calc.spatial.index(X, Y, cell.size)]
  # Count how many points fall into each cell
  pc[, Npoints := .N, by=CellNo]
  # Calculate how many samples are possible in each cell. If the
  # total number of points in the cell is less than the desired
  # number, then all points should be sampled
  pc[, Nsamples := pmin(Npoints, returns.per.cell)]
  # Sample Nsamples random points from each cell
  # Solution from: https://stackoverflow.com/questions/41042750/how-do-you-sample-data-within-each-group-in-a-data-table-fastest-way-possible
  pc.out <- pc[pc[, .I[sample(.N, Nsamples)], by=CellNo][[2]],]
  # Remove the columns created during thinning process
  pc.out <- subset(pc.out, select=-c(CellNo, Npoints, Nsamples))
  return(pc.out)
}
