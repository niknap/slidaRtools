#' Convert raster to XYZ-data.frame
#'
#' Convert a raster to a XYZ-data.frame with Z being the pixel values
#' @param ras Input raster object
#' @return data.frame
#' @keywords raster conversion coordinate XYZ table data.frame
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

ras2xyzdf <- function(ras){
  #require(raster)
  #require(reshape2)
  # Extract the resolution of the raster in X- and Y-direction
  resx <- res(ras)[1]
  resy <- res(ras)[2]
  # Extract the coordinates of the lower left corner of the raster
  minx <- xmin(ras)
  miny <- ymin(ras)
  # Convert the raster to a matrix
  mx <- raster::as.matrix(ras)
  # Transpose and resort the matrix to account for the different
  # coordinate conventions of rasters and matrices
  mx <- t(mx[nrow(mx):1, ])
  # Melt the matrix to a data.frame and assign column names
  xyz <- data.frame(melt(mx))
  names(xyz) <- c("X", "Y", "Z")
  # Multiply the matrix coordinates with the raster resolution 
  # and add the values of the lower left raster corner
  # to get the original coordinate values back
  xyz$X <- xyz$X * resx + minx
  xyz$Y <- xyz$Y * resy + miny
  return(xyz)
}












