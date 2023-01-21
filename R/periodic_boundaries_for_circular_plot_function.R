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



#' Periodic boundaries for circular forest plots
#'
#' Function that creates periodic boundaries for voxelforests along circular plot borders
#' (e.g. large lidar footprints or circular inventory plots). Tree crown voxels
#' that exceed the boundaries of the circle re-appear on the opposite side
#' of the circle. The placement of trees is based on the polar coordinates of
#' the stem positions (distance and angle to the circle center). All trees with
#' stems inside the circle are copied to exist a second time outside of the
#' circle at the position opposite of the circle center and with the same distance
#' to the boundary, but outwards.
#' @param vxf.dt Data.table containing voxelforest object created with make.voxelforest; columns of the stem positions Xstem and Ystem are crucial
#' @param Xctr X-coordinate of the plot center
#' @param Yctr Y-coordinate of the plot center
#' @param radius of the circular plot
#' @param clip Boolean to specify whether output should be clipped to the circle (default) or all
#' @return data.table containing the voxelforest with periodic boundaries
#' @keywords periodic boundaries border edge effects closed wrapped world tree crowns point cloud voxelforest large footprint circle plot
#' @export
#' @examples in progress
#' @author Nikolai Knapp

periodic_boundaries_for_circular_plot <- function(vxf.dt, Xctr, Yctr, radius, clip=T){

  # Make sure to use data.table format
  require(data.table)
  vxf.dt <- data.table(vxf.dt)

  # Subset ground voxels inside the circle
  gr.dt <- subset(vxf.dt, Z == 0 & in_circle(X, Y, Xctr, Yctr, radius))

  # Subset only trees with stems inside the circle
  vxf.dt <- subset(vxf.dt, in_circle(Xstem, Ystem, Xctr, Yctr, radius))

  # Duplicate the voxelforest
  vxf.dt2 <- copy(vxf.dt)

  # Avoid division by zero by shifting trees that are directly
  # in the circle center by 0.1 meters
  vxf.dt2[Xstem == Xctr, Xstem := Xstem + 0.1]
  vxf.dt2[Ystem == Yctr, Ystem := Ystem + 0.1]

  # Calculate the distance and the angle of the stem to the center
  # for the duplicated voxelforest
  vxf.dt2[, StemCenterDistance := ((Xstem-Xctr)^2+(Ystem-Yctr)^2)^0.5]
  vxf.dt2[, StemCenterAngle := atan((Ystem-Yctr)/(Xstem-Xctr))]
  vxf.dt2[Xstem < Xctr, StemCenterAngle := StemCenterAngle + pi]
  vxf.dt2[Xstem >= Xctr & Ystem < Yctr, StemCenterAngle := StemCenterAngle + 2*pi]
  vxf.dt2[is.na(StemCenterAngle), StemCenterAngle := 0]
  vxf.dt2[, NewDistance := StemCenterDistance]
  vxf.dt2[, NewAngle := StemCenterAngle]

  # Calculate the new X and Y coordinates of the stems outside of the circle
  vxf.dt2[, NewDistance := radius-StemCenterDistance+radius]
  vxf.dt2[StemCenterAngle < pi, NewAngle := StemCenterAngle+pi]
  vxf.dt2[StemCenterAngle >= pi, NewAngle := StemCenterAngle-pi]
  vxf.dt2[, NewXstem := round(Xctr + NewDistance * cos(NewAngle))]
  vxf.dt2[, NewYstem := round(Yctr + NewDistance * sin(NewAngle))]

  # Calculate the new coordinates for all voxels
  vxf.dt2[, X := X - Xstem + NewXstem]
  vxf.dt2[, Y := Y - Ystem + NewYstem]

  # Combine the original and the shifted voxelforest
  vxf.dt3 <- rbind(gr.dt, vxf.dt, vxf.dt2, fill=T)

  # Remove the additional columns
  rmcols <- c("StemCenterDistance", "StemCenterAngle", "NewDistance", "NewAngle", "NewXstem", "NewYstem")
  vxf.dt3 <- subset(vxf.dt3, select=!(names(vxf.dt3) %in% rmcols))

  # Clip with circular footprint
  if(clip == T){
    vxf.dt3 <- subset(vxf.dt3, in_circle(X, Y, Xctr, Yctr, radius))
  }

  # Return the result
  return(vxf.dt3)
}
