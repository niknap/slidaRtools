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
#' @author Nikolai Knapp, nikolai.knapp@ufz.de


make.profile.from.Z <- function(Z.vec, binwidth=1){
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



