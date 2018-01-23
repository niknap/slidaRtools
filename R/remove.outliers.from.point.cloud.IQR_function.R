#' Remove outliers from point cloud
#'
#' Function that removes points from a point cloud if their Z-value exceeds
#' the given percentile of their neighborhood's Z-values by a certain factor.
#' @param pc Input point cloud (first 3 columns need to be "X", "Y" and "Z")
#' @param n Number of considered neighbor points (with lowest XY-distance to focal point)
#' @param low.perc Lower end of the desired interquantile range (fraction between 0 and 1)
#' @param high.perc Upper end of the desired interquantile range (fraction between 0 and 1)
#' @param fac Factor by which a Z-value needs to exceed or subceed the IQR of its neighbors to be considered an outlier
#' @param high bool if false high outliers will not be removed
#' @param low bool if false low outliers will not be removed
#' @return Point cloud dataframe with rows from the input that were detected as outliers removed
#' @keywords point cloud outlier filter clean percentile IQR
#' @export
#' @examples in progress


remove.outliers.from.point.cloud.IQR <- function(pc, n=50, low.perc=0.25, high.perc=0.75, fac=1.5, high=T, low=T){

  require(RANN)

  # Function that calculates a percentile for a given vector x of values
  low.perc.fun <- function(x, na.rm=T){
    return(quantile(x, probs=low.perc, na.rm=T))
  }
  high.perc.fun <- function(x, na.rm=T){
    return(quantile(x, probs=high.perc, na.rm=T))
  }

  # Find all neighbors for all points considering only horizontal distance (X and Y)
  neighbor.list <- find.nearest.neighbors(pc=pc[, c(1, 2)], n=n)

  # Add the Z-coordinates, which were not used for distance calculation, to the
  # neighbors list
  zcor.mx <- matrix(pc[neighbor.list[[1]], 3], nrow=nrow(pc), ncol=n)

  # Calculate the percentiles of Z-values in the neigborhood of each point
  low.perc.vec <- apply(zcor.mx, 1, low.perc.fun)
  high.perc.vec <- apply(zcor.mx, 1, high.perc.fun)
  # Create an interquantile vector
  IQR.vec <- high.perc.vec - low.perc.vec
  # Prepare a boolean outlier vector
  outlier.vec <- rep(F, times=nrow(pc))

  # Identify a point as high outlier if its Z-value exceeds the high quantile
  # of its neighbors by the factor times the IQR
  if(high==T){
    high.outlier.vec <- pc$Z > high.perc.vec + fac * IQR.vec
    outlier.vec <- ifelse((outlier.vec == T | high.outlier.vec == T), T, F)
  }
  # Identify a point as low outlier if its Z-value subceeds the low quantile
  # of its neighbors by the factor times the IQR
  if(low==T){
    low.outlier.vec <- pc$Z < low.perc.vec - fac * IQR.vec
    outlier.vec <- ifelse((outlier.vec == T | low.outlier.vec == T), T, F)
  }

  # Remove rows of outliers from the dataframe
  pc <- pc[outlier.vec == F,]
  return(pc)
}







