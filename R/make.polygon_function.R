#' Make a polygon
#'
#' Function that creates a spatial polygon data frame from given vertex coordinates.
#' @param vertex.mx Matrix with two columns (X- and Y-coordinates) where each row represents one vertex
#' @return SpatialPolygonsDataFrame
#' @keywords vector shape polygon vertex area
#' @export
#' @examples test.spdf <- make.polygon(vertex.mx=matrix(c(1,1,5,1,3,3), ncol=2, byrow=T))
#' plot(test.spdf)

make.polygon <- function(vertex.mx){
  require(sp)
  require(rgeos)
  # Add the first vertex also to the end, to create a closed polygon
  vertex.mx <- rbind(vertex.mx, vertex.mx[1,])
  # Make a spatial polygon
  p = Polygon(vertex.mx)
  ps = Polygons(list(p), 1)
  sps = SpatialPolygons(list(ps))
  df = data.frame(ID=1)
  spdf = SpatialPolygonsDataFrame(sps, df)
  return(spdf)
}


