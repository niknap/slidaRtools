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



#' Convert a point cloud to a raster
#'
#' Function that creates a raster from a point cloud by applying a certain function
#' in each cell, e.g. func=max returns a surface raster (CHM in case of vegetation),
#' min returns lowest point per cell, mean returns the mean value of all points in the
#' cell, count returns a point density raster, sum returns the sum of all point values
#' and mode returns the most common value (in case of several equally common values one of
#' them is taken randomly).
#' @param pc Point cloud in XYZ-table format
#' @param xcor Variable name that contains the X-coordinates (Default "X")
#' @param ycor Variable name that contains the Y-coordinates (Default "Y")
#' @param var Variable name that contains the values that should be rasterized (Default "Z")
#' @param res Resolution (cell side length) of the output raster
#' @param func Function to be applied (either "max", "min", "mean", "count", "sum" or "mode")
#' @return Raster object
#' @keywords raster rasterization point cloud XYZ CHM
#' @export
#' @examples in progress
#' @author Nikolai Knapp

raster_from_point_cloud <- function(pc, xcor="X", ycor="Y", var="Z", res=1, func="max"){
  require(plyr)
  require(reshape2)
  require(raster)
  # Error handling
  possibleError <- tryCatch({
    # Data conversion
    pc <- as.data.frame(pc)
    x.vec <- pc[, xcor]
    y.vec <- pc[, ycor]
    z.vec <- pc[, var]
    # Derive the data's extent
    minx <- round_any(min(x.vec), accuracy=res, f=floor)
    miny <- round_any(min(y.vec), accuracy=res, f=floor)
    maxx <- round_any(max(x.vec), accuracy=res, f=ceiling)
    maxy <- round_any(max(y.vec), accuracy=res, f=ceiling)
    # Remove X- and Y-values that fall directly on the respective maxima
    new.x.vec <- x.vec[x.vec != maxx & y.vec != maxy]
    new.y.vec <- y.vec[x.vec != maxx & y.vec != maxy]
    z.vec <- z.vec[x.vec != maxx & y.vec != maxy]
    # Calculate the new X- and Y-coordinate
    new.x.vec <- round_any(new.x.vec, accuracy=res, f=floor) / res
    new.y.vec <- round_any(new.y.vec, accuracy=res, f=floor) / res
    df <- data.frame(cbind(X=new.x.vec, Y=new.y.vec, Z=z.vec))
    # Function that returns the mode (most common value) in the data
    mode.func <- function(vec, na.rm=T){
      #counts <- ifelse(na.rm == T, table(vec), table(vec, useNA="ifany"))
      counts <- table(vec)
      maxima <- which(counts == max(counts))
      n.maxima <- length(maxima)
      values <- as.vector(as.numeric(names(maxima)))
      random.element <- sample(n.maxima, size=1)
      return(values[random.element])
    }
    # Cast the dataframe with the new coordinates to a matrix using the
    # desired function as aggregation function
    if(func == "max"){
      suppressWarnings(mx <- acast(data=df, X~Y, value.var="Z", fun.aggregate=max))
    } else if(func == "min"){
      suppressWarnings(mx <- acast(data=df, X~Y, value.var="Z", fun.aggregate=min))
    } else if(func == "count"){
      suppressWarnings(mx <- acast(data=df, X~Y, value.var="Z", fun.aggregate=length))
    } else if(func == "mean"){
      suppressWarnings(mx <- acast(data=df, X~Y, value.var="Z", fun.aggregate=mean))
    } else if(func == "sum"){
      suppressWarnings(mx <- acast(data=df, X~Y, value.var="Z", fun.aggregate=sum))
    } else if(func == "mode"){
      suppressWarnings(mx <- acast(data=df, X~Y, value.var="Z", fun.aggregate=mode.func, summarize, fill=0))
    }
    # Replace -Inf by NA
    mx[mx[] == -Inf] <- NA
    # Convert the "by-column-matrix" to raster (requires coordinate transposition)
    mx <- t(mx[, ncol(mx):1])
    ras <- raster(mx, xmx=ncol(mx), ymx=nrow(mx))
    # Adjust the extent of the raster (otherwise pixels are shifted by 0.5 times pixel size)
    ras <- setExtent(ras, ext=extent(c(minx, maxx, miny, maxy)))
    #ras <- setExtent(ras, ext=extent(c(minx, maxx+res, miny, maxy+res)))
    #ras <- crop(ras, extent(c(minx, maxx, miny, maxy)))
    return(ras)
    # Error handling
  }, error=function(e) e)
  if(inherits(possibleError, "error")){
    # In this block some code that should be executed after
    # errors can be inserted.
    # In case an error occurs somewhere, return a raster with the single cell value NA
    ras <- raster(matrix(NA))
    return(ras)
  }
}
