#' Fill raster gaps
#'
#' Function that fills gaps in a raster.
#' @param in.ras Input raster
#' @param fill.window.size Radius of the window around the focal pixel from which a filling value is calculated
#' @param fill.window.type Shape of the window must be "circle" or "rectangle"
#' @param fill.value Replacing value, e.g. "mean", "min", "max", of the filling window or simply 0 or NA
#' @return Filled raster object
#' @keywords raster rasterization gap hole NA fill replace CHM window
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de


gapfill <- function(in.ras, fill.window.size=5, fill.window.type="circle", fill.value="mean"){
  #require(raster)
  # Create the moving window matrix
  xdim <- 2*fill.window.size+1
  ydim <- 2*fill.window.size+1
  mx <- matrix(0, nrow=xdim, ncol=ydim)
  if(fill.window.type == "rectangle"){
    mx[] <- 1
  } else if(fill.window.type == "circle"){
    in.circle <- function(Xarray, Yarray, Xct, Yct, radius){
      resultarray <- ifelse((Xarray - Xct)^2 + (Yarray - Yct)^2 <= radius^2, 1, 0)
      return(resultarray)
    }
    xmx <- matrix(rep(1:xdim, each=ydim), nrow=xdim, ncol=ydim)
    ymx <- matrix(rep(1:xdim, times=ydim), nrow=xdim, ncol=ydim)
    mx <- in.circle(xmx, ymx, fill.window.size+1, fill.window.size+1, fill.window.size)
  }
  # Create functions for mean and min that ignore 0
  mean.ignore.0 <- function(vec, na.rm=T){
    vec <- vec[vec > 0]
    return(mean(vec, na.rm=na.rm))
  }
  min.ignore.0 <- function(vec, na.rm=T){
    vec <- vec[vec > 0]
    return(min(vec, na.rm=na.rm))
  }
  # Fill the gaps
  if(fill.value == "mean"){
    filled.ras <- focal(in.ras, w=mx, fun=mean.ignore.0, na.rm=T, pad=T, padValue=NA, NAonly=T)
  } else if(fill.value == "min"){
    filled.ras <- focal(in.ras, w=mx, fun=min.ignore.0, na.rm=T, pad=T, padValue=NA, NAonly=T)
  } else if(fill.value == "max"){
    filled.ras <- focal(in.ras, w=mx, fun=max, na.rm=T, pad=T, padValue=NA, NAonly=T)
  } else if(fill.value == 0){
    filled.ras <- in.ras
    filled.ras[is.na(filled.ras[])] <- 0
  } else if(fill.value == "NA"){
    filled.ras <- in.ras
  }
  return(filled.ras)
}






