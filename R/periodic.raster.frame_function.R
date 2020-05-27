#' Periodic raster frame
#'
#' Function that increases a raster by adding a frame around in which the
#' pixel values correspond to the values found at the opposite side of the
#' raster, creating periodic boundaries, e.g. for moving window analysis.
#' @param ras Raster object
#' @param w Width of the frame (number of pixels)
#' @return raster object
#' @keywords raster frame buffer margin periodic boundaries edge effect moving window filter texture
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

periodic.raster.frame <- function(ras, w=10){
  #require(raster)

  # Memorize extent and resolution of input raster
  minx <- raster::xmin(ras)
  maxx <- raster::xmax(ras)
  miny <- raster::ymin(ras)
  maxy <- raster::ymax(ras)
  xres <- raster::res(ras)[1]
  yres <- raster::res(ras)[2]

  # Increase the extent by the desired frame
  new.extent <- raster::extent(minx-xres*w, maxx+xres*w, miny-yres*w, maxy+yres*w)
  new.ras <- raster::extend(ras, new.extent)

  # Subset the right, left, lower and upper frame raster
  left.ras <- raster::crop(ras, extent(minx, minx+w*xres, miny, maxy))
  right.ras <- raster::crop(ras, extent(maxx-xres*w, maxx, miny, maxy))
  lower.ras <- raster::crop(ras, extent(minx, maxx, miny, miny+w*yres))
  upper.ras <- raster::crop(ras, extent(minx, maxx, maxy-yres*w, maxy))

  # Shift them
  left.ras <- raster::setExtent(left.ras, extent(minx+maxx, maxx+w*xres, miny, maxy))
  right.ras <- raster::setExtent(right.ras, extent(minx-w*xres, minx, miny, maxy))
  lower.ras <- raster::setExtent(lower.ras, extent(minx, maxx, miny+maxy, maxy+w*yres))
  upper.ras <- raster::setExtent(upper.ras, extent(minx, maxx, miny-w*yres, miny))

  # Mosaic them
  new.ras <- raster::mosaic(ras, left.ras, right.ras, lower.ras, upper.ras, fun=mean)

  # Subset the corners
  ll.ras <- raster::crop(ras, extent(minx, minx+w*xres, miny, miny+w*yres))
  lr.ras <- raster::crop(ras, extent(maxx-w*xres, maxx, miny, miny+w*yres))
  ul.ras <- raster::crop(ras, extent(minx, minx+w*xres, maxy-w*yres, maxy))
  ur.ras <- raster::crop(ras, extent(maxx-w*xres, maxx, maxy-w*yres, maxy))

  # Shift them
  ll.ras <- raster::setExtent(ll.ras, extent(maxx, maxx+w*xres, maxy, maxy+w*yres))
  lr.ras <- raster::setExtent(lr.ras, extent(minx-w*xres, minx, maxy, maxy+w*yres))
  ul.ras <- raster::setExtent(ul.ras, extent(maxx, maxx+w*xres, miny-w*yres, miny))
  ur.ras <- raster::setExtent(ur.ras, extent(minx-w*xres, minx, miny-w*yres, miny))

  # Mosaic them
  new.ras <- raster::mosaic(new.ras, ll.ras, lr.ras, ul.ras, ur.ras, fun=mean)

  # plot(new.ras)
  return(new.ras)
}


















