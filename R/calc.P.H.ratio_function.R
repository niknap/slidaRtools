#' Calculate P:H ratio
#'
#' Function that calculates the ratio P:H of a canopy profile (Marvin et al. 2014),
#' where P is the height of the maximal peak in the profile (highest return
#' or foliage density) and H is the maximal height of the profile.
#' @param prof Lidar profile as vector with height bins as names
#' @return number P:H
#' @keywords height profile lidar point cloud CHM xyz
#' @export
#' @examples in progress
calc.P.H.ratio <- function(prof){
  if(length(prof) > 1){
    # Exclude ground
    prof <- prof[2:length(prof)]
    # Find value of maximal peak in profile
    peak.value <- max(prof)
    # Find height of maximal peak and if there are more than one take the highest
    peak.h <- max(as.numeric(as.character(which(prof==peak.value))), na.rm=T)
    # Find the maximal height
    max.h <- max(as.numeric(as.character(names(prof))), na.rm=T)
    # Calculate the ratio
    ratio <- peak.h/max.h
    return(ratio)
  } else {
    return(0)
  }
}





