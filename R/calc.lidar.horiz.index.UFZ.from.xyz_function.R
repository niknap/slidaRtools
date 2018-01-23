#' Calculate horizontal structure index from Lidar point cloud
#'
#' Function for horizontal index from UFZ (inspired by TUM index for field data) 
#' calculated from Lidar XYZ-table data (point cloud or CHM)
#' @param XYZtable Lidar point cloud or CHM in XYZ-table format
#' @return Index of horizontal forest structure
#' @keywords forest structure horizontal heterogeneity Lidar XYZ
#' @export
#' @examples in progress

calc.lidar.horiz.index.UFZ.from.xyz <- function(XYZtable){
  require(data.table)
  XYZtable <- data.table(XYZtable)
  h.max <- max(XYZtable$Z)
  if(h.max > 0){
    h.75 <- 0.75*h.max
    ret1 <- nrow(XYZtable[Z >= h.75, ])
    ret.all <- nrow(XYZtable)
    # Relative cover of pixels above 75% of max. height
    # (corresponds to N/ha in SDI)
    rel.cov <- (ret1/ret.all)
    # Quadratic mean pixel height (excluding ground pixels)
    # (corresponds to quadratic mean diameter dg in SDI)
    h.vec <- XYZtable[Z > 0, Z]
    qmh <- (mean(h.vec^2))^0.5
    # Apply the SDI equation replacing N/ha with rel. cover and dg with qmh
    pseudo.SDI <- rel.cov*(25/qmh)^(-1.605)
    lidar.horiz.index.TUM <- max(1-pseudo.SDI, 0)
    return(lidar.horiz.index.TUM)
  } else {
    return(0)
  }
}

