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



#' Find nearest neighbors in point cloud
#'
#' Function that finds the n nearest neighbor points for each point
#' in a point cloud of up to 3 dimensions
#' @param pc Point cloud as data.frame with at least a column "X" and optional columns "Y" and "Z"
#' @param n Number of neighbors to find (1 only returns the focal point itself)
#' @return list with up to five elements which are all matrices where each row represents
#' each focal point in the point cloud and each column represents one neighbor sorted by
#' increasing distance. The matrix elements represent 1) neighbor indices, 2) neighbor distances,
#' 3) neighbor X-coodinates, 4) neighbor Y-coodinates, 5) neighbor Z-coodinates.
#' @keywords nearest neighbor search find ANN distance coordinates
#' @export
#' @examples in progress


find.nearest.neighbors <- function(pc, n=2){
  require(RANN)
  dimensions <- ncol(pc)
  res.list <- nn2(data=pc, k=n)
  index.mx <- res.list[[1]]
  dist.mx <- res.list[[2]]
  xcor.mx <- matrix(pc[index.mx, 1], nrow=nrow(pc), ncol=n)
  res.list[[3]] <- xcor.mx
  names(res.list)[3] <- "nn.xcor"
  if(ncol(pc) > 1){
    ycor.mx <- matrix(pc[index.mx, 2], nrow=nrow(pc), ncol=n)
    res.list[[4]] <- ycor.mx
    names(res.list)[4] <- "nn.ycor"
  }
  if(ncol(pc) > 2){
    zcor.mx <- matrix(pc[index.mx, 3], nrow=nrow(pc), ncol=n)
    res.list[[5]] <- zcor.mx
    names(res.list)[5] <- "nn.zcor"
  }
  return(res.list)
}










