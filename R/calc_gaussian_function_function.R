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



#' Calculate Gaussian function value for a point
#'
#' Function that returns for each given point the value of a Gaussian function.
#' @param pt Coordinate of the point
#' @param amp Amplitude of the peak of the Gaussian curve
#' @param ctr Coordinate of the center point
#' @param sd Standard deviation of the Gaussian curve (for a curve that contains >99\% of the area between -1 and 1 use sd=0.4)
#' @return Vector of function values for each input point
#' @keywords Gaussian density weighting bell curve normal distribution
#' @export
#' @examples in progress
#' @author Nikolai Knapp

calc_gaussian_function <- function(pt, amp=1, ctr=0, sd=1){
  output <- amp*exp(-(pt-ctr)^2/(2*sd^2))
  return(output)
}
