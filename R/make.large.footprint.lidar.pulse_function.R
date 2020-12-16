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



#' Scan a voxel forest with a virtual large footprint Lidar
#'
#' Function that simulates a Lidar waveform of a) a given voxel forest or b) a given lidar point
#' cloud or CHM and returns a profile vector.
#' The horizontal energy distribution inside the pulse is simulated with a Gaussian distribution.
#' @param xyz.dt Input data.table which can either be a) a voxel forest created with make.voxelforest function (containing columns X, Y, Z and LAI among others) or b) a discrete lidar point cloud or CHM (containing columns X, Y and Z)
#' @param input Switch for the input type, which has to be either "vxf" for voxel forest or "lid" for discrete lidar data
#' @param Xctr X-coordinate of the pulse center
#' @param Yctr Y-coordinate of the pulse center
#' @param diameter Diameter of the pulse footprint in meters (e.g. LVIS = 25 m, SLICER = 10 m, Icesat GLAS = 70 m, GEDI = 25 m)
#' @param binwidth Vertical resolution of the waveform in meters
#' @param k Extinction coefficient (only relevant if input = "vxf")
#' @param sd Standard deviation of the Gaussian energy distribution (for a curve that approaches 0 at the margin of the pulse use sd = 0.4\*radius; for GEDI simulations commonly sd = 0.5\*radius is used, which leads to a more truncated energy distribution)
#' @param VG.ratio Reflectance ratio between vegetation surface and ground surface
#' @param weight.var Weighting variable, e.g. set to "Intensity" to use the intensity column in a lidar point cloud
#' @return Vector of the Lidar waveform (normalized to a sum of 1)
#' @keywords voxel forest plot lidar large footprint pulse waveform profile gaussian simulation
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.large.footprint.lidar.pulse <- function(xyz.dt, input="vxf", Xctr, Yctr, diameter=25, binwidth=1, k=0.2, sd=0.5*12.5, VG.ratio=2.5, weight.var=NA){
  require(data.table)
  xyz.dt <- data.table(xyz.dt)
  # Cut out the voxels that fall into the pulse cylinder
  sub.dt <- xyz.dt[in.circle(xyz.dt$X, xyz.dt$Y, Xctr, Yctr, diameter/2) == T]
  # Calculate intensity for each point. The absolute value is irrelevant, because the final profile will be normalized to a sum of 1.
  if(input=="vxf"){
    # Calculate relative intensity for each voxel using Beer-Lambert light extinction based on LAI.
    sub.dt[, I := exp(-k*LAI)]
  }else if(input=="lid"){
    # Assign each point an initial intensity of 1
    sub.dt[, I := 1]
  }
  # If desired multiply the intensity with a given weighting variable
  if(!is.na(weight.var)){
    sub.dt[, I := I * get(weight.var)]
  }
  # Adjust the intensity at ground level, considering the vegetation vs. ground refectance ratio
  sub.dt[Z == 0, I := I/VG.ratio]
  # Weight voxels in the pulse center higher than at the margins using a 2D Gaussian weighting function.
  sub.dt$weight <- calc.gaussian.function.2D(Xpt=sub.dt$X, Ypt=sub.dt$Y, amp=1, Xctr=Xctr, Yctr=Yctr, sd=sd)
  # Multiply each voxel's intensity with its weight
  sub.dt[, I := I*weight]
  # Aggregate the XYZ-table into a vertical profile vector
  sub.dt <- subset(sub.dt, select=c("X", "Y", "Z", "I"))
  sub.df <- data.frame(sub.dt)
  prof <- make.profile.from.XYZ.value(sub.df, binwidth, stat="sum")
  # Normalize the profile
  prof <- prof/sum(prof)
  return(prof)
}







