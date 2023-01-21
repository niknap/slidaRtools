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



#' Derive a vertical profile of a vector of heights
#'
#' Function that creates profiles from Z-vectors with a custom height
#' resolution (binwidth)
#' @param Z.vec vector containing single height measurements
#' @param binwidth number specifying the vertical width of height classes
#' @return vector of point or pixel counts per height class with height values as names
#' @keywords profile lidar point cloud CHM xyz height Z vector
#' @export
#' @examples in progress
#' @author Nikolai Knapp

make_profile_from_Z <- function(Z.vec, binwidth=1){
  require(plyr)
  max.Z <- max(Z.vec, na.rm=T)
  min.Z <- min(Z.vec, na.rm=T)
  Z.vec <- round_any(Z.vec, binwidth, floor)
  Z.summary <- table(Z.vec)
  profile.bins <- seq(round_any(min.Z, binwidth, floor), round_any(max.Z, binwidth, ceiling), binwidth)
  profile.vals <- rep(0, times=length(profile.bins))
  profile.vals[match(names(Z.summary), profile.bins)] <- Z.summary
  names(profile.vals) <- profile.bins
  return(profile.vals)
}



