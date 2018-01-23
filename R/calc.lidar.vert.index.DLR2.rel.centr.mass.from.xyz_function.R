#' Calculate vertical structure index suggestion of Victor Cazcarra (DLR)
#' from Lidar point cloud
#'
#' Function for vertical index from DLR calculated from Lidar
#' XYZ-table data (point cloud or CHM).
#' The equation for the index V is: V = 1 - (abs(CM - Hperc) / Hperc) with
#' CM being the center of mass (equal to MCH or TCH, depending if input is
#' point cloud or CHM-raster) and Hperc being a custom fraction of the max.
#' height (Hmax) in the plot (default 50 percent).
#' @param XYZtable Lidar point cloud or CHM in XYZ-table format
#' @param perc Fraction between 0 and 1 (default 0.5) to specify Hperc = perc * Hmax
#' @param h.min Minimum height below which Lidar returns are ignored
#' @return Index of vertical forest structure
#' @keywords forest structure vertical heterogeneity Lidar XYZ
#' @export
#' @examples in progress

calc.lidar.vert.index.DLR2.rel.centr.mass.from.xyz <- function(XYZtable, perc=0.5, h.min=0){
  require(data.table)
  XYZtable <- data.table(XYZtable)
  XYZtable <- subset(XYZtable, Z >= h.min)
  h.max <- max(XYZtable$Z, na.rm=T)
  # Check if there are any returns that exceed the minimum height threshold
  # otherwise return 0
  if(nrow(XYZtable) > 0 & h.max > 0){
    h.perc <- perc*h.max
    # Calculate center of mass+
    CM <- mean(XYZtable$Z, na.rm=T)
    # Calculate the vertical index
    V.index <- 1 - (abs(CM - h.perc) / h.perc)
    return(V.index)
  } else {
    return(0)
  }
}

