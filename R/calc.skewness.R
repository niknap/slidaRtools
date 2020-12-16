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



#' Calculate Skewness
#'
#' Function for calculation of skewness of an empirical distribution
#' @param vec Vector of data
#' @return Skewness (negative = left-skewed / positive = right-skewed)
#' @keywords skewness distribution shape profile mean symmetry
#' @export
#' @examples calc.skewness(c(1,2,3,3,3,4))
#' @author Nikolai Knapp

calc.skewness <- function(vec){
  vec.mean <- mean(vec, na.rm=T)
  vec.sd <- sd(vec, na.rm=T)
  return(sum(((vec-vec.mean)/vec.sd)^3)/length(vec))
}
