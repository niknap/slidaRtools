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
  require(reshape2)
  require(abind)
  XYZ.dt <- data.table(XYZ.df)
  XYZ.dt <- subset(XYZ.dt, select=c("X", "Y", "Z", value.var))
  # Round the coordinates to full voxel units
  XYZ.dt$X <- round(XYZ.dt$X/res.xy)*res.xy
  XYZ.dt$Y <- round(XYZ.dt$Y/res.xy)*res.xy
  XYZ.dt$Z <- round(XYZ.dt$Z/res.z)*res.z
  # Cast the data.table with the new coordinates to an array using the
  # desired function as aggregation function
  if(func == "max"){
    suppressWarnings(cast.array.3D <- acast(data=XYZ.dt, X~Y~Z, value.var=value.var, fun.aggregate=max, na.rm=T))
  } else if(func == "min"){
    suppressWarnings(cast.array.3D <- acast(data=XYZ.dt, X~Y~Z, value.var=value.var, fun.aggregate=min, na.rm=T))
  } else if(func == "count"){
    suppressWarnings(cast.array.3D <- acast(data=XYZ.dt, X~Y~Z, value.var=value.var, fun.aggregate=length))
  } else if(func == "mean"){
    suppressWarnings(cast.array.3D <- acast(data=XYZ.dt, X~Y~Z, value.var=value.var, fun.aggregate=mean, na.rm=T))
  } else if(func == "sum"){
    suppressWarnings(cast.array.3D <- acast(data=XYZ.dt, X~Y~Z, value.var=value.var, fun.aggregate=sum, na.rm=T))
  } else if(func == "bool"){
    suppressWarnings(cast.array.3D <- acast(data=XYZ.dt, X~Y~Z, value.var=value.var, fun.aggregate=length))
    cast.array.3D[cast.array.3D > 0] <- 1
    cast.array.3D[cast.array.3D <= 0] <- 0
  }
  # Replace -Inf, Inf, NaN and NA by 0
  cast.array.3D[is.infinite(cast.array.3D)] <- 0
  cast.array.3D[is.na(cast.array.3D)] <- 0
  # If a global max. height is desired, that exceeds the max. height of the point cloud
  # add additional empty voxel layers on top of the array
  if(!is.na(global.h.max)){
    h.max <- ceiling(global.h.max/res.z)*res.z
  } else {
    h.max <- max(XYZ.dt$Z)
  }
  # Create empty array that covers the whole space (the casted array
  # only contains those slices for which there was data in the input XYZ-table)
  namesx <- seq(min(XYZ.dt$X), max(XYZ.dt$X), res.xy)
  namesy <- seq(min(XYZ.dt$Y), max(XYZ.dt$Y), res.xy)
  namesz <- seq(min(XYZ.dt$Z), h.max, res.z)
  extx <- (max(XYZ.dt$X)-min(XYZ.dt$X))/res.xy+1
  exty <- (max(XYZ.dt$Y)-min(XYZ.dt$Y))/res.xy+1
  extz <- (max(XYZ.dt$Z)-min(XYZ.dt$Z))/res.z+1
  array.3D <- array(data=0, dim=c(length(namesx), length(namesy), length(namesz)),
                       dimnames=list(namesx, namesy, namesz))
  # Write the values from the casted array to the total array
  abind::afill(array.3D) <- cast.array.3D
  return(array.3D)
}


















