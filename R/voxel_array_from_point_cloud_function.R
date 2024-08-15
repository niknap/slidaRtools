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



#' Convert a point cloud to a voxel array
#'
#' Function that takes a point cloud in XYZ-data.frame format and returns
#' a 3D array of custom horizontal and vertical voxel resolution.
#' @param XYZ.df Lidar point cloud in XYZ-data.frame format
#' @param global.h.max Maximal Z of the array (if not set, the maximal Z of the point cloud is taken)
#' @param res.xy Horizontal sidelength of the voxels
#' @param res.z Vertical sidelength of the voxels
#' @param value.var Variable (data.frame column) from which to derive the voxel values
#' @param fun Aggregation function ("bool", "count", "min", "mean", "max", "sum")
#' @return 3D array
#' @keywords forest structure voxel array empty space canopy volume Lidar point cloud XYZ
#' @export
#' @examples arr <- voxel.array.from.point.cloud(XYZ.df=pc, res.xy=10, res.z=1)
#' @author Nikolai Knapp

voxel_array_from_point_cloud <- function(XYZ.df, global.h.max=NA, res.xy=5, res.z=5, value.var="Z", func="count"){
  require(data.table)
  XYZ.dt <- data.table(XYZ.df)
  XYZ.dt <- subset(XYZ.dt, select=c("X", "Y", "Z", value.var))
  # Round the coordinates to full voxel units
  XYZ.dt$X <- round(XYZ.dt$X/res.xy)*res.xy
  XYZ.dt$Y <- round(XYZ.dt$Y/res.xy)*res.xy
  XYZ.dt$Z <- round(XYZ.dt$Z/res.z)*res.z
  # Cast the data.table with the new coordinates to an array using the
  # desired function as aggregation function
  if(func == "max"){
    agg.dt <- XYZ.dt[, .(agg.val = max(Z, na.rm=T)), keyby=c("X", "Y", "Z")]
  } else if(func == "min"){
    agg.dt <- XYZ.dt[, .(agg.val = min(Z, na.rm=T)), keyby=c("X", "Y", "Z")]
  } else if(func == "count"){
    agg.dt <- XYZ.dt[, .(agg.val = .N), keyby=c("X", "Y", "Z")]
  } else if(func == "mean"){
    agg.dt <- XYZ.dt[, .(agg.val = mean(Z, na.rm=T)), keyby=c("X", "Y", "Z")]
  } else if(func == "sum"){
    agg.dt <- XYZ.dt[, .(agg.val = sum(Z, na.rm=T)), keyby=c("X", "Y", "Z")]
  } else if(func == "bool"){
    agg.dt <- XYZ.dt[, .(agg.val = .N), keyby=c("X", "Y", "Z")]
    agg.dt[, agg.val := 1]
  }
  # Replace -Inf, Inf, NaN and NA by 0
  agg.dt[is.infinite(agg.val), agg.val := 0]
  agg.dt[is.na(agg.val), agg.val := 0]
  # If a global max. height is desired, that exceeds the max. height of the point cloud
  # add additional empty voxel layers on top of the array
  if(!is.na(global.h.max)){
    h.max <- ceiling(global.h.max/res.z)*res.z
  } else {
    h.max <- max(XYZ.dt$Z)
  }
  # Expand a complete grid of all cells
  complete.x.vec <- seq(min(XYZ.dt$X), max(XYZ.dt$X), res.xy)
  complete.y.vec <- seq(min(XYZ.dt$Y), max(XYZ.dt$Y), res.xy)
  complete.z.vec <- seq(min(XYZ.dt$Z), h.max, res.z)
  grid.dt <- CJ(X=complete.x.vec, Y=complete.y.vec, Z=complete.z.vec)
  # Merge all aggregated values to the complete grid
  grid.dt <- merge(grid.dt, agg.dt, all.x=T, by=c("X", "Y", "Z"))
  # Sort by coordinate columns in the order that is necessary to later write
  # the data into an array with X varying faster than Y varying faster than Z
  setorderv(grid.dt, cols=c("Z", "Y", "X"))
  # Create empty array that covers the whole space (the casted array
  # only contains those slices for which there was data in the input XYZ-table)
  extx <- (max(XYZ.dt$X)-min(XYZ.dt$X))/res.xy+1
  exty <- (max(XYZ.dt$Y)-min(XYZ.dt$Y))/res.xy+1
  extz <- (max(XYZ.dt$Z)-min(XYZ.dt$Z))/res.z+1
  array.3D <- array(data=grid.dt$agg.val,
                    dim=c(length(complete.x.vec), length(complete.y.vec), length(complete.z.vec)),
                    dimnames=list(complete.x.vec, complete.y.vec, complete.z.vec))
  # Replace NA by 0
  array.3D[is.na(array.3D)] <- 0
  return(array.3D)
}


















