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



#' Create a circular moving window matrix
#'
#' Function that creates a circular moving window matrix.
#' @param rad Radius of the considered (circular) neighborhood
#' @return matrix with 1 in circle and NA outside the circle
#' @keywords raster focal moving window filter circle
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.circular.moving.window.matrix <- function(rad=10){
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
  return(mx)
}
