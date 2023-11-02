# Copyright (C) 2023 Dr. Nikolai Knapp, Thünen Institute
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



#' Reconstruct leaf area density (LAD) from lidar point clouds
#'
#' Function to reconstruct leaf area density (LAD) per volume based on the
#' MacArthur-Horn equation, which inverts the Beer-Lambert principle of
#' light extinction. It provides an estimate of LAD per voxel.
#'
#' @param pc Point cloud in data.table, data.frame or LAS format
#' (important: column names need to be X, Y and Z)
#' @param xy.res Side length of a voxel in the horizontal plain
#' @param z.res Side length of a voxel in vertical direction
#' @param k Conversion coefficient (light extinction coefficient)
#' @param min.out For voxels with zero outgoing pulses, the LAD cannot be
#' calculated from the MH equation (division by 0), thus these voxels would
#' get NA. However, if there are return in them, there has to be leaf area in
#' them. min.out allows to specify a minimum number of outgoing pulses
#' for such cases, to not lose the leaf area in these voxels.
#' @param normalized Boolean whether the point cloud is terrain normalized or
#' not. For normalized point clouds all points of height < 0.5 m are considered
#' ground points. For non-normalized point clouds a column "Classification" is
#' required and ground points should be labelled.
#' @param ground.class Only required for non-normalized point clouds. It
#' provides the numeric code of ground points in the "Classification" column.
#' The standard value is 2.
#' @return data.table with each row representing one voxel. Coordinates
#' represent voxel centers and LAD and cumulative LAI are given for each voxel
#' @export
#' @examples in progress
#' @author Nikolai Knapp


leaf_area_density <- function(pc, xy.res=1, z.res=1, k=1, min.out=1,
                              normalized=F, ground.class=2){

  # # Debug
  # require(data.table)
  # pc=lid.dt
  # xy.res=10
  # z.res=1
  # k=1
  # min.out=1
  # normalized=T

  # Check input format and if necessary convert to data.table
  input.class <- class(pc)[1]
  if(input.class == "LAS"){
    require(lidR)
    pc.las <- pc
    pc <- copy(pc.las@data)
  }else if(input.class == "data.frame"){
    pc <- data.table(pc)
  }

  # Round coordinates to res meters precision
  pc[, X_floor := floor(X/xy.res)*xy.res]
  pc[, Y_floor := floor(Y/xy.res)*xy.res]
  pc[, Z_floor := floor(Z/z.res)*z.res]

  # Create a unique ID for each xy cell
  pc[, ID_unit := .GRP, by=.(X_floor, Y_floor)]

  # Label ground points
  pc[, Ground_point := F]
  if(normalized == F){
    pc[Classification == ground.class, Ground_point := T]
  }else{
    pc[Z < 0.5, Ground_point := T]
  }

  # Calculate the number of points in each voxel
  vox.dt <- pc[, .(N_in_voxel = .N), keyby=c("X_floor", "Y_floor", "Z_floor")]

  # Calculate the number of ground points in each voxel
  ground.dt <- pc[Ground_point == T, .(N_ground_in_voxel = .N), keyby=c("X_floor", "Y_floor", "Z_floor")]

  # Merge ground points to the voxels
  vox.dt <- merge(vox.dt, ground.dt, all.x=T, by=c("X_floor", "Y_floor", "Z_floor"))
  vox.dt[is.na(N_ground_in_voxel), N_ground_in_voxel := 0]

  # Sort the voxels in the order X-, Y- and Z-coordinate
  setorderv(vox.dt, c("X_floor", "Y_floor", "Z_floor"), c(1, 1, 1))

  # Calculate the number of pulses entering a voxel as the cumulative sum
  # of all points in and below the voxel
  vox.dt[, N_entering_voxel := cumsum(N_in_voxel), by=c("X_floor", "Y_floor")]

  # Calculate the number of pulses leaving a voxel by subtracting the number
  # of points in the voxel from the number of pulses entering the voxel. Ground
  # points are not leaf area and therefore they are always considered as
  # leaving the voxel, even though they are in the ground voxels
  vox.dt[, N_leaving_voxel := N_entering_voxel - N_in_voxel + N_ground_in_voxel]

  # In the cases where zero pulses are leaving a voxel, replace the number
  # with the number given by min.out, to avoid NAs for such voxels
  vox.dt[N_leaving_voxel == 0, N_leaving_voxel := min.out]

  # Apply the MacArthur-Horn equation to convert point counts to LAD
  vox.dt[, LAD := ifelse(N_leaving_voxel > 0, log(N_entering_voxel/N_leaving_voxel)*1/z.res*1/k, NA)]

  # Rename the coordinate columns
  setnames(vox.dt, old=c("X_floor", "Y_floor", "Z_floor"), new=c("X", "Y", "Z"))

  # Memorize where LAD is NA due to occluded voxels
  na.vec <- is.na(vox.dt$LAD)

  # Temporarily replace NAs in LAD column to calculate cumulative sum (LAI)
  vox.dt[is.na(LAD), LAD := 0]

  # Calculate cumulative leaf area index for each voxel
  vox.dt[, LAI := cumsum(LAD), by=c("X", "Y")]

  # Change LAD of occluded voxels back to NA
  vox.dt[na.vec, LAD := NA]

  # Shift coordinates to voxel centers
  vox.dt[, X := X + xy.res/2]
  vox.dt[, Y := Y + xy.res/2]
  vox.dt[, Z := Z + z.res/2]

  return(vox.dt)
}







