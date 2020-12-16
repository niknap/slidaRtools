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



#' Calculate Gaussian function value for a point in 2D space
#'
#' Function that returns for each X-Y-coordinate pair the value of a Gaussian function.
#' @param Xpt X-coordinate of the point
#' @param Ypt Y-coordinate of the point
#' @param amp Amplitude of the peak of the Gaussian curve
#' @param Xctr X-coordinate of the center point
#' @param Yctr Y-coordinate of the center point
#' @param sd Standard deviation of the Gaussian curve (for a curve that contains >99\% of the area between -1 and 1 use sd=0.4)
#' @return Vector of function values for each input point
#' @keywords Gaussian density weighting 2D bell curve normal distribution
#' @export
#' @examples in progress

calc.gaussian.function.2D <- function(Xpt, Ypt, amp=1, Xctr=0, Yctr=0, sd=1){
  dist <- ((Xpt-Xctr)^2+(Ypt-Yctr)^2)^0.5
  output <- amp*exp(-(dist)^2/(2*sd^2))
  return(output)
}
