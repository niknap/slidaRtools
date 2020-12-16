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



#' Calculate mean top-of-canopy height (TCH) from XYZ-dataframe
#'
#' Function to calculate TCH from the point cloud in XYZ-dataframe format for a custom pixel size.
#' TCH is the mean of all CHM pixel values. So within each pixel the highest
#' return is taken and then the average height over all pixels is calculated.
#' @param XYZdf Point cloud in XYZ-dataframe format
#' @param pxl.res Side length of CHM pixel
#' @return mean top-of-canopy height (TCH)
#' @keywords TCH CHM mean forest height Lidar XYZ profile raster top-of-canopy pixel resolution scale
#' @export
#' @examples in progress

calc.TCH.from.point.cloud <- function(XYZdf, pxl.res=1){
  XYZdf <- data.frame(XYZdf)
  names(XYZdf)[1:3] <- c("X", "Y", "Z")
  if((nrow(XYZdf) > 0) & (F %in% is.na(XYZdf$Z))){
    chm.ras <- raster.from.point.cloud(XYZdf, res=pxl.res, func="max")
    TCH <- cellStats(chm.ras, stat="mean", na.rm=T)
    if(is.na(TCH)){
      TCH <- NA
    }
    return(TCH)
  } else {
    return(NA)
  }
}










