#' Calculate horizontal structure index from Lidar point cloud
#'
#' Function for horizontal index from DLR calculated from Lidar
#' XYZ-table data (point cloud or CHM)
#' @param XYZtable Lidar point cloud or CHM in XYZ-table format
#' @param global.h.max Maximal tree height in the region for normalization (Default=50)
#' @return Index of horizontal forest structure
#' @keywords forest structure horizontal heterogeneity Lidar XYZ
#' @export
#' @examples in progress

calc.lidar.horiz.index.DLR.from.xyz <- function(XYZtable, global.h.max=50){
  require(data.table)
  XYZtable <- data.table(XYZtable)
  h.max <- max(XYZtable$Z)
  h.60 <- 0.6*h.max
  ret1 <- nrow(XYZtable[Z >= h.60, ])
  ret.all <- nrow(XYZtable)
  lidar.horiz.index.DLR <- (ret1/ret.all)*h.max/global.h.max
  return(lidar.horiz.index.DLR)
}


