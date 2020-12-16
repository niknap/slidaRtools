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



#' Make a X-coordinate array
#'
#' Create a 3D array that contains the X-coordinate of each cell as cell value
#' @param minx minimal X-coordinate
#' @param maxx maximal X-coordinate
#' @param miny minimal Y-coordinate
#' @param maxy maximal Y-coordinate
#' @param minz minimal Z-coordinate
#' @param maxz maximal Z-coordinate
#' @param res side length of one cell
#' @return 3D array of X-coordinates
#' @keywords X coordinate array 3D voxel
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.xcor.array <- function(minx=0, maxx, miny=0, maxy, minz=0, maxz, res=1){
  nx <- ceiling((maxx-minx) / res)
  ny <- ceiling((maxy-miny) / res)
  nz <- ceiling((maxz-minz) / res)
  arr <- array(data=rep(1:nx, times=ny*nz), dim=c(nx, ny, nz))
  return(arr)
}
