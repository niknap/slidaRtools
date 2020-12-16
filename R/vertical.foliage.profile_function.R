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



#' Derive a vertical foliage profile (VFP) from a Lidar profile
#'
#' Function that returns the vertical foliage profile for a given Lidar profile
#' using the approach of Harding et al. (2001) which is based on the MacArthur-Horn
#' method.
#' @param profile Vector of Lidar profile (with heights as names)
#' @param h.bin Vertical extent of one height bin
#' @param GR.threshold Height threshold below which everything is regarded as ground
#' return (GR) and ignored in the processing
#' @param k Light extinction coefficient, which is an adjustment factor from relative
#' to absolute (LAD) VFP
#' @return Vector of the vertical foliage profile (VFP)
#' @keywords profile correction adjustment MacArthur Horn LAI LAD leaf area foliage vegetation canopy cover occlusion
#' @export
#' @examples prof.vec <- c(9, 2, 4, 5, 6, 3, 1)
#' names(prof.vec) <- 0:6
#' vfp.vec <- vertical.foliage.profile(profile=prof.vec, h.bin=1, GR.threshold=1, k=0.5)
#' plot(names(prof.vec) ~ prof.vec, type="l", xlab="Lidar returns", ylab="Height [m]")
#' plot(names(vfp.vec) ~ vfp.vec, type="l", xlab="LAD [m2/m3]", ylab="Height [m]")
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

vertical.foliage.profile <- function(profile, h.bin=1, GR.threshold=5, k=0.5){
  # Calculate P(hi), the relative cumulative Lidar profile or gap fraction
  # profile, which is 1 at canopy top and approaches the relative share of
  # ground returns in all returns at the bottom
  rev.profile <- rev(profile)
  cum.profile <- 1-rev(cumsum(rev.profile))/sum(profile)

  # Calculate V(hi), the vegetation profile, vertical foliage profile (VFP) or
  # leaf area density (LAD) profile, using the MacArthur-Horn method which is
  # a logarithmic form of Beer-Lambert light extinction.
  cum.profile.next <- cum.profile
  cum.profile.next[1:length(cum.profile)] <- c(cum.profile[2:(length(cum.profile))], 1)
  VFP <- profile
  VFP[] <- 0
  VFP[2:length(VFP)] <- (1/(k*h.bin))*(log(cum.profile.next[2:length(cum.profile.next)])-log(cum.profile[2:length(cum.profile)]))

  # Set all values below the GR.threshold to 0
  VFP[as.numeric(as.character(names(VFP))) < GR.threshold] <- 0
  return(VFP)
}




