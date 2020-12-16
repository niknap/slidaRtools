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



#' Derive a vertical profile of a point cloud for a certain variable
#'
#' Function that creates profiles of a certain statistic of a certain variable
#' from XYZ-tables with a custom height resolution (binwidth)
#' @param XYZ.value.table 4-column data frame with 3rd column containing heights and 4th column containing values for which to make the profile
#' @param binwidth number specifying the vertical width of height classes
#' @param stat desired statistic ("sum", "count", "max", "min", "mean", "median")
#' @return vector of point or pixel counts per height class with height values as names
#' @keywords profile lidar point cloud CHM xyz
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.profile.from.XYZ.value <- function(XYZ.value.table, binwidth=1, stat="sum"){
  require(plyr)
  XYZ.value.table <- data.frame(XYZ.value.table)
  names(XYZ.value.table) <- c("X", "Y", "Z", "value")
  max.Z <- max(XYZ.value.table$Z)
  min.Z <- min(XYZ.value.table$Z)
  XYZ.value.table$Z <- round_any(XYZ.value.table$Z, binwidth, floor)
  XYZ.value.table <- data.table(XYZ.value.table)
  if(stat == "sum"){
    val.summary <- XYZ.value.table[, .(Stat=sum(value, na.rm=T)), by='Z']
  } else if(stat == "max"){
    val.summary <- XYZ.value.table[, .(Stat=max(value, na.rm=T)), by='Z']
  } else if(stat == "min"){
    val.summary <- XYZ.value.table[, .(Stat=min(value, na.rm=T)), by='Z']
  } else if(stat == "mean"){
    val.summary <- XYZ.value.table[, .(Stat=mean(value, na.rm=T)), by='Z']
  } else if(stat == "median"){
    val.summary <- XYZ.value.table[, .(Stat=quantile(value, 0.5, na.rm=T)), by='Z']
  } else if(stat == "count"){
    val.summary <- XYZ.value.table[, .(Stat=.N), by='Z']
  }
  profile.bins <- seq(round_any(min.Z, binwidth, floor), round_any(max.Z, binwidth, ceiling), binwidth)
  profile.vals <- rep(0, times=length(profile.bins))
  profile.vals[match(val.summary$Z, profile.bins)] <- val.summary$Stat
  names(profile.vals) <- profile.bins
  return(profile.vals)
}



