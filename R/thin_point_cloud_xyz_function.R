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



#' Point cloud thinning in 1D, 2D or 3D
#'
#' Function that thins a point cloud to a desired homogenous
#' point density by keeping a random subset of n points
#' in each spatial unit and discarding all other points.
#' Spatial units are defined by their dimensions, i.e. 3D voxels (xyz),
#' 2D cells (most commonly in xy plane), or can even be just along one
#' dimension (e.g. slices in z direction).
#' @param pc Point cloud in data.table, data.frame or LAS format (important: column names need to be X, Y and Z)
#' @param res Side length of a spatial unit (equal in all dimensions)
#' @param n Number of points to keep in each cell
#' @param dim Axes that define the thinning units, e.g. "xyz" for 3D voxels, "xy" for 2D grid cells in the horizontal plane,...
#' @return Thinned point cloud in same format as input
#' @export
#' @examples in progress
#' @author Nikolai Knapp


thin_point_cloud_xyz <- function(pc, res=1, n=4, dim="xy"){
  require(data.table)

  # # Debug
  # pc=my.las
  # res=1
  # n=20
  # dim="xyz"


  # Check input format and if necessary convert to data.table
  input.class <- class(pc)[1]
  if(input.class == "LAS"){
    require(lidR)
    pc.las <- pc
    pc <- copy(pc.las@data)
  }else if(input.class == "data.frame"){
    pc <- data.table(pc)
  }

  # Make an explicit copy of the input pc using the same name. This is
  # necessary, otherwise manipulations done inside this function
  # (e.g. adding columns) will be applied to the input table passed as pc
  # argument
  # https://stackoverflow.com/questions/44661961/dont-want-original-data-table-to-be-modified-when-passed-to-a-function
  pc <- copy(pc)

  # Round coordinates to res meters precision
  pc[, X_round := floor(X/res)*res]
  pc[, Y_round := floor(Y/res)*res]
  pc[, Z_round := floor(Z/res)*res]

  # # Make a list of all dimensions which are chosen for the thinning
  # column.list <-

  # Create a unique unit ID depending on which dimensions are chosen
  # for thinning
  if(dim == "xyz"){
    pc[, ID_unit := .GRP, by=.(X_round, Y_round, Z_round)]
  }else if(dim == "xy"){
    pc[, ID_unit := .GRP, by=.(X_round, Y_round)]
  }else if(dim == "xz"){
    pc[, ID_unit := .GRP, by=.(X_round, Z_round)]
  }else if(dim == "yz"){
    pc[, ID_unit := .GRP, by=.(Y_round, Z_round)]
  }else if(dim == "x"){
    pc[, ID_unit := .GRP, by=.(X_round)]
  }else if(dim == "y"){
    pc[, ID_unit := .GRP, by=.(Y_round)]
  }else if(dim == "z"){
    pc[, ID_unit := .GRP, by=.(Z_round)]
  }

  # Count how many points there are per unit
  pc[, N_points := .N, by=ID_unit]
  # Calculate how many samples are possible in each unit: If the
  # total number of points in the unit is less than the desired
  # number, then all points should be sampled
  pc[, N_samples := pmin(N_points, n)]
  # Check which values for N_samples occur in the data
  n.samples.vec <- sort(unique(pc$N_samples))

  # Sample N_samples random points from each unit which has at least n points
  # Solution from: https://stackoverflow.com/questions/41042750/how-do-you-sample-data-within-each-group-in-a-data-table-fastest-way-possible
  my.sampled.rows.vec <- pc[N_samples == n, .I[sample(x=.N, size=n)], by=ID_unit][[2]]
  # Take all points, which are in units with less than n points
  all.lower.density.rows.vec <- which(pc$N_points < n)
  # Alternative data.table syntax (is not faster)
  # all.lower.density.rows.vec <- pc[, .I[N_points < n]]
  # Combine the indices of all rows for subsetting
  subset.rows.vec <- sort(c(my.sampled.rows.vec, all.lower.density.rows.vec))

  # Prepare the output based on the input's format
  if(input.class == "LAS"){

    # Subset only the sampled points
    pc.las@data$PointRowID <- 1:nrow(pc.las@data)
    pc.out <- filter_poi(pc.las, PointRowID %in% subset.rows.vec)
    suppressMessages(pc.out <- remove_lasattribute(pc.out, "PointRowID"))
    # pc.out.las <- LAS(pc.out)

  }else{

    # Subset the sampled points
    pc.out <- pc[subset.rows.vec, ]
    # Remove the extra columns created during thinning process
    pc.out <- subset(pc.out, select=-c(X_round, Y_round, Z_round,
                                       ID_unit, N_points, N_samples))

    # If necessary convert back to data.frame
    if(input.class == "data.frame"){
      pc.out <- data.frame(pc.out)
    }

  }
  return(pc.out)
}







