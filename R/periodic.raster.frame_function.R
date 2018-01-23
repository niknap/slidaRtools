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
  require(raster)
  
  # Memorize extent and resolution of input raster
  minx <- xmin(ras)
  maxx <- xmax(ras)
  miny <- ymin(ras)
  maxy <- ymax(ras)
  xres <- res(ras)[1]
  yres <- res(ras)[2]

  # Increase the extent by the desired frame
  new.extent <- extent(minx-xres*w, maxx+xres*w, miny-yres*w, maxy+yres*w)
  new.ras <- extend(ras, new.extent)

  # Subset the right, left, lower and upper frame raster
  left.ras <- crop(ras, extent(minx, minx+w*xres, miny, maxy))
  right.ras <- crop(ras, extent(maxx-xres*w, maxx, miny, maxy))
  lower.ras <- crop(ras, extent(minx, maxx, miny, miny+w*yres))
  upper.ras <- crop(ras, extent(minx, maxx, maxy-yres*w, maxy))
  
  # Shift them
  left.ras <- setExtent(left.ras, extent(minx+maxx, maxx+w*xres, miny, maxy))
  right.ras <- setExtent(right.ras, extent(minx-w*xres, minx, miny, maxy))
  lower.ras <- setExtent(lower.ras, extent(minx, maxx, miny+maxy, maxy+w*yres))
  upper.ras <- setExtent(upper.ras, extent(minx, maxx, miny-w*yres, miny))
  
  # Mosaic them
  new.ras <- mosaic(ras, left.ras, right.ras, lower.ras, upper.ras, fun=mean)
  
  # Subset the corners
  ll.ras <- crop(ras, extent(minx, minx+w*xres, miny, miny+w*yres))
  lr.ras <- crop(ras, extent(maxx-w*xres, maxx, miny, miny+w*yres))
  ul.ras <- crop(ras, extent(minx, minx+w*xres, maxy-w*yres, maxy))
  ur.ras <- crop(ras, extent(maxx-w*xres, maxx, maxy-w*yres, maxy))
  
  # Shift them
  ll.ras <- setExtent(ll.ras, extent(maxx, maxx+w*xres, maxy, maxy+w*yres))
  lr.ras <- setExtent(lr.ras, extent(minx-w*xres, minx, maxy, maxy+w*yres))
  ul.ras <- setExtent(ul.ras, extent(maxx, maxx+w*xres, miny-w*yres, miny))
  ur.ras <- setExtent(ur.ras, extent(minx-w*xres, minx, miny-w*yres, miny))
  
  # Mosaic them
  new.ras <- mosaic(new.ras, ll.ras, lr.ras, ul.ras, ur.ras, fun=mean)
 
  # plot(new.ras)
  return(new.ras)
}


















