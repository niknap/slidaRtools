# Copyright (C) 2017 Dr. Nikolai Knapp, UFZ
#
# This file is part of the slidaRtools R package.
#
# The slidaRtools R package is free software: you can redistribute
# it and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# slidaRtools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with slidaRtools If not, see <http://www.gnu.org/licenses/>.



#' Remove outliers from raster
#'
#' Function that sets value of a raster pixel to NA if it exceeds or subceeds
#' the interquantile range (IQR) of its neighbors by a certain factor.
#' @param ras Input raster
#' @param rad Radius of the considered (circular) neighborhood
#' @param low.perc Lower end of the desired interquantile range (fraction between 0 and 1)
#' @param high.perc Upper end of the desired interquantile range (fraction between 0 and 1)
#' @param fac Factor by which a pixel value needs to exceed or subceed the IQR of its neighbors to be considered an outlier
#' @param high bool if false high outliers will not be removed
#' @param low bool if false low outliers will not be removed
#' @return Raster object with gaps (NA) at the positions of former outliers
#' @keywords raster outlier filter CHM DTM clean quantile quartile percentile interquantile range IQR
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de


remove.outliers.from.raster.IQR <- function(ras, rad=5, low.perc=0.25, high.perc=0.75, fac=1.5, high=T, low=T){

  # Function that calculates a percentile for a given vector x of values
  low.perc.fun <- function(x, na.rm=T){
    return(quantile(x, probs=low.perc, na.rm=T))
  }
  high.perc.fun <- function(x, na.rm=T){
    return(quantile(x, probs=high.perc, na.rm=T))
  }

  # Create a circular moving window matrix
  xdim <- 2*rad+1
  ydim <- 2*rad+1
  mx <- matrix(NA, nrow=xdim, ncol=ydim)
  in.circle <- function(Xarray, Yarray, Xct, Yct, radius){
    resultarray <- ifelse((Xarray - Xct)^2 + (Yarray - Yct)^2 <= radius^2, 1, NA)
    return(resultarray)
  }
  xmx <- matrix(rep(1:xdim, each=ydim), nrow=xdim, ncol=ydim)
  ymx <- matrix(rep(1:xdim, times=ydim), nrow=xdim, ncol=ydim)
  mx <- in.circle(xmx, ymx, rad+1, rad+1, rad)

  # Calculate the percentile for each pixel in a neigborhood of the given radius
  low.perc.ras <- focal(ras, w=mx, fun=low.perc.fun, na.rm=T, pad=T, padValue=NA, NAonly=F)
  # Calculate the percentile for each pixel in a neigborhood of the given radius
  high.perc.ras <- focal(ras, w=mx, fun=high.perc.fun, na.rm=T, pad=T, padValue=NA, NAonly=F)
  # Create an interquantile raster
  IQR.ras <- high.perc.ras - low.perc.ras

  # Set a pixel value to NA if it exceeds the high quantile
  # of its neighbors by the factor times the IQR
  if(high==T){
    ras[ras[] > high.perc.ras[]+fac*IQR.ras[]] <- NA
  }
  # Set a pixel value to NA if it subceeds the low quantile
  # of its neighbors by the factor times the IQR
  if(low==T){
    ras[ras[] < low.perc.ras[]-fac*IQR.ras[]] <- NA
  }
  return(ras)
}


















