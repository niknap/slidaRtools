#' Make rasters from values and spatial indices
#'
#' Function that returns a raster based on given spatial indices (plot numbers)
#' and corresponding values that should be assigned to each pixel.
#' @param spatial.ID vector containing spatial indices
#' @param value vector containing values to fill the pixels
#' @param res side length of one spatial subunit
#' @param minx minimum X-coordinate
#' @param miny minimum Y-coordinate
#' @param maxx maximum X-coordinate
#' @param maxy maximum Y-coordinate
#' @return raster object
#' @keywords spatial index plot number raster map value cast
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.map.from.spatial.index <- function(spatial.ID, value, res=1, minx=NA, miny=NA, maxx=NA, maxy=NA){
  # Count number of cells in X- and Y-direction
  # %/%: integer division operator
  nx <- (maxx - minx) %/% res
  ny <- (maxy - miny) %/% res
  # Build the matrix
  id.mx <- matrix(1:(nx*ny), nrow=ny, ncol=nx, byrow=T)
  # Fill the matrix with values
  value.mx <- id.mx
  value.mx[] <- NA
  value.mx[match(spatial.ID, id.mx)] <- value
  # Convert matrix to raster
  ras <- rowmx2ras(value.mx)
  # Adjust the raster extent
  ras <- setExtent(ras, ext=c(minx, maxx, miny, maxy))
  return(ras)
}
