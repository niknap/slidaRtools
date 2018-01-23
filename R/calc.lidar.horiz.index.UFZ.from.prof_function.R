#' Calculate horizontal structure index from Lidar profile
#'
#' Function for horizontal index from UFZ (inspired by TUM index for field data) 
#' calculated from Lidar vertical profile data
#' @param prof Lidar profile as vector with height bins as names
#' @return Index of horizontal forest structure
#' @keywords forest structure horizontal heterogeneity Lidar profile
#' @export
#' @examples in progress

calc.lidar.horiz.index.UFZ.from.prof <- function(prof){
  if(length(prof) > 1){
    h.vec <- as.numeric(as.character(names(prof)))
    h.max <- max(h.vec)
    h.75 <- 0.75*h.max
    ret1 <- sum(prof[h.vec >= h.75])
    ret.all <- sum(prof)
    # Relative cover of pixels above 75% of max. height
    # (corresponds to N/ha in SDI)
    rel.cov <- (ret1/ret.all)
    # Quadratic mean weighted profile height (excluding ground return)
    # (corresponds to quadratic mean diameter dg in SDI)
    h.vec <- h.vec[-1]
    prof <- prof[-1]
    qmh <- (sum(prof*h.vec^2) / sum(prof))^0.5
    # Apply the SDI equation replacing N/ha with rel. cover and dg with qmh
    pseudo.SDI <- rel.cov*(25/qmh)^(-1.605)
    lidar.horiz.index.TUM <- max(1-pseudo.SDI, 0)
    return(lidar.horiz.index.TUM)
  } else {
    return(0)
  } 
}


