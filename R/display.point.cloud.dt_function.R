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



#' Plot point cloud in 3D space
#'
#' Function for 3-dimensional point cloud visualization.
#' @param pc Point cloud in XYZ-table format (important: column names need to be X, Y and Z)
#' @param col.palette Color palette
#' @param col.var String with name of the column which contains the information for coloring
#' @param col.lim Vector of two elements specifying the limits of the col.var values
#' @param size ...of plotted points
#' @return 3D plot in rgl window
#' @import data.table
#' @export
#' @keywords 3D plot display point cloud graphics visualization lidar
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

display.point.cloud.dt <- function(xyz.dt, col.palette=NA, col.var="Z", col.lim=NA, size=1){
  # Make sure input is data.table
  xyz.dt <- data.table(xyz.dt)
  # If no custom color palette is provided, create and apply a blue to red heat color palette
  if(is.na(col.palette)){
    col.palette <- colorRampPalette(c("darkblue", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"), space = "Lab")(50)
  }
  # If no custom color limits are provided, stretch them between min and max of the Z values
  if(is.na(col.lim[1])){
    col.lim <- c(floor(min(xyz.dt$Z, na.rm=T)), ceiling(max(xyz.dt$Z, na.rm=T)))
  }
  # Rescale the raw values to a color index vector that contains the indices of the
  # color vector as whole numbers
  col.index <- round((length(col.palette)-1) * (xyz.dt[, get(col.var)]-col.lim[1]) / (col.lim[2]-col.lim[1]))+1
  # Create 3D plot with coloring according to height
  rgl::bg3d("black")
  rgl::plot3d(xyz.dt$X, xyz.dt$Y, xyz.dt$Z, col=col.palette[col.index],
              aspect=F, axes=F, box=F, xlab="", ylab="", zlab="", size=size)
}


