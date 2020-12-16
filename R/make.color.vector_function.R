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



#' Make a color vector
#'
#' Function that takes a vector of values and a desired color palette and returns
#' color values that correspond to each vector value.
#' @param vec Value vector
#' @param col.palette Color palette, e.g. default R palettes or created with colorRampPalette function
#' @param min Minimum value to consider for the scaling of the color values (may subceed the value vector)
#' @param max Maximum value to consider for the scaling of the color values (may exceed the value vector)
#' @param step Step width between values to consider for the scaling of the color values
#' @param alpha Transparency from 0 = fully transparent to 1 = fully opaque
#' @return Vector of color codings with the same length and order as the input vector
#' @keywords colors palette values coding ramp scale
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.color.vector <- function(vec, palette=NA, min=NA, max=NA, step=1, alpha=1){
  require(grDevices)
  # If no custom color palette is provided, create and apply a blue to red heat color palette
  suppressWarnings(
    if(is.na(palette)){
      palette <- colorRampPalette(c("darkblue", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"), space = "Lab")
    })
  # If no extrema are provided, take the extrema of the vector
  if(is.na(min)){
    min <- min(vec, na.rm=T)
  }
  if(is.na(max)){
    max <- max(vec, na.rm=T)
  }
  breaks.vec <- seq(min-step, max, step)
  class.vec <- cut(vec, breaks=breaks.vec)
  col.vec <- palette(length(breaks.vec)-1)[as.numeric(class.vec)]
  col.vec <- adjustcolor(col.vec, alpha.f=alpha)
  return(col.vec)
}


