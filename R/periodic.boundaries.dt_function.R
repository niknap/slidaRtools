#' Periodic boundaries
#'
#' Function that creates periodic boundaries for point clouds, i.e. points
#' that exceed the boundaries of the area re-appear on the opposite side 
#' of the area. 
#' @param xyz.dt Data.table containing X-, Y- and Z-coordinates of the points
#' @param minx Minimum X-coordinate of the area
#' @param maxx Maximum X-coordinate of the area
#' @param miny Minimum Y-coordinate of the area
#' @param maxy Maximum Y-coordinate of the area
#' @return data.table of X-, Y- and Z-coordinates with periodic boundaries
#' @keywords periodic boundaries border edge effects closed wrapped world tree crowns point cloud
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

periodic.boundaries.dt <- function(xyz.dt, minx, maxx, miny, maxy){
  require(data.table)
  xyz.dt <- data.table(xyz.dt)
  # Calculate the extent in both horizontal dimensions
  extx <- maxx - minx
  exty <- maxy - miny
  # Calculate the new X- and Y-values using modulo division
  xyz.dt[, X := (X - minx) %% extx + minx]
  xyz.dt[, Y := (Y - miny) %% exty + miny]
  return(xyz.dt)
}


