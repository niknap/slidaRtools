#' Calculate fractional canopy cover (FCC) from XYZ-dataframe
#'
#' Function to calculate FCC from the point cloud in XYZ-dataframe format for a
#' custom pixel size and height threshold.
#' FCC is the relative share of all CHM pixels values above a height threshold.
#' The CHM is generated internally from the point cloud by rasterizing the highest
#' return in each pixel of a given resolution.
#' @param XYZdf Point cloud in XYZ-dataframe format
#' @param pxl.res Side length of CHM pixel
#' @param h.threshold Height threshold (pixels with the exact threshold value are included as canopy)
#' @return Fractional canopy cover (FCC) as a value between 0 and 1
#' @keywords fractional canopy cover gap fraction FCC CHM forest height Lidar XYZ threshold raster pixel resolution relative proportion
#' @export
#' @examples in progress

calc.FCC.from.point.cloud <- function(XYZdf, pxl.res=1, h.threshold=1){
  XYZdf <- data.frame(XYZdf)
  names(XYZdf)[1:3] <- c("X", "Y", "Z")
  if((nrow(XYZdf) > 0) & (F %in% is.na(XYZdf$Z))){
    chm.ras <- raster.from.point.cloud(XYZdf, res=pxl.res, func="max")
    FCC <- length(chm.ras[chm.ras >= h.threshold])/length(chm.ras)
    if(is.na(FCC)){
      FCC <- NA
    }
    return(FCC)
  } else {
    return(NA)
  }
}






