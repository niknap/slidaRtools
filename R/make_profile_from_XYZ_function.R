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



#' Derive a vertical profile of a point cloud or a canopy height model
#'
#' Function that creates profiles from XYZ-tables with a custom height
#' resolution (binwidth)
#' @param XYZ.table 3-column data frame with 3rd column containing heights
#' @param binwidth number specifying the vertical width of height classes
#' @return vector of point or pixel counts per height class with height values as names
#' @keywords profile lidar point cloud CHM xyz
#' @export
#' @examples in progress
#' @author Nikolai Knapp

make_profile_from_XYZ <- function(XYZ.table, binwidth=1){
  require(plyr)
  require(data.table)
  XYZ.table <- data.frame(XYZ.table)
  names(XYZ.table) <- c("X", "Y", "Z")
  max.Z <- max(XYZ.table$Z, na.rm=T)
  min.Z <- min(XYZ.table$Z, na.rm=T)
  XYZ.table$Z <- round_any(XYZ.table$Z, binwidth, floor)
  XYZ.table <- data.table(XYZ.table)
  Z.summary <- XYZ.table[,.N, by='Z']
  profile.bins <- seq(round_any(min.Z, binwidth, floor), round_any(max.Z, binwidth, ceiling), binwidth)
  profile.vals <- rep(0, times=length(profile.bins))
  profile.vals[match(Z.summary$Z, profile.bins)] <- Z.summary$N
  names(profile.vals) <- profile.bins
  return(profile.vals)
}




