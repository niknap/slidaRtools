#' Make a vectorgrid
#'
#' Function that creates as vectorgrid of rectangular cells.
#' @param ll.x X-coordinate of the lower left corner
#' @param ll.y Y-coordinate of the lower left corner
#' @param size.x Side length of one cell in X-direction
#' @param size.y Side length of one cell in Y-direction
#' @param n.x Number of cells in X-direction
#' @param n.y Number of cells in Y-direction
#' @param rot.angle Rotation angle clockwise in degrees
#' @return SpatialPolygonsDataFrame
#' @keywords vector grid shape fishnet sampling matrix lattice polygon
#' @export
#' @examples testgrid.spdf <- make.vectorgrid(ll.x=0, ll.y=0, size.x=2, size.y=3, n.x=10, n.y=5)
#' plot(testgrid.spdf)
#' sub.spdf <- subset(testgrid, ID==1)
#' plot(sub.spdf, add=T, col="red")
#' sub.spdf <- subset(testgrid, ID==2)
#' plot(sub.spdf, add=T, col="green")
#' sub.spdf <- subset(testgrid, ID==11)
#' plot(sub.spdf, add=T, col="blue")

make.vectorgrid <- function(ll.x=0, ll.y=0, size.x=1, size.y=1, n.x=1, n.y=1, rot.angle=0){
  require(sp)
  require(maptools)
  # Create an empty list to collect the single polygons
  ps.list <- list()
  # Loop over all unique polygons and process the coordinate info to build a polygon
  i <- 1
  for(i.y in 1:n.y){
    # Calculate y-coordinates of all 4 corners
    my.ll.y <- ll.y + i.y * size.y - size.y
    my.lr.y <- ll.y + i.y * size.y - size.y
    my.ul.y <- ll.y + i.y * size.y
    my.ur.y <- ll.y + i.y * size.y
    for(i.x in 1:n.x){
      # Calculate x-coordinates of all 4 corners
      my.ll.x <- ll.x + i.x * size.x - size.x
      my.ul.x <- ll.x + i.x * size.x - size.x
      my.ur.x <- ll.x + i.x * size.x
      my.lr.x <- ll.x + i.x * size.x
      # Build a corner matrix
      corner.mx <- matrix(c(my.ll.x, my.ll.y, my.ul.x, my.ul.y,
                            my.ur.x, my.ur.y, my.lr.x, my.lr.y,
                            my.ll.x, my.ll.y), ncol=2, byrow=T)
      # Make a polygon
      p = sp::Polygon(corner.mx)
      ps = sp::Polygons(list(p), 1)
      # Add it to the Polygons list (by default IDs > 1e5 are written in scientific
      # notation; this is avoided with format function; this is needed for later
      # conversion to SpatialPolygonsDataFrame)
      ps.list[[i]] <- sp::Polygons(list(p), ID=format(i, scientific=F))
      i <- i+1
    }
  }
  # Convert them to spatial polygons
  sps <- sp::SpatialPolygons(ps.list)
  # Convert them to a spatial polygon dataframe
  spdf <- sp::SpatialPolygonsDataFrame(sps, data.frame(ID=1:(i-1)))
  # Rotate the spatial polygons dataframe
  spdf <- maptools::elide(spdf, rotate=rot.angle)
  return(spdf)
}
