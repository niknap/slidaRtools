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



#' Calculate values of a discrete power law distribution
#'
#' Function that returns values of a discrete Pareto (power law)
#' distribution density (e.g. canopy gap sizes, tree diameters)
#' for a given scaling parameter lambda;
#' Code adapted from Asner et al. (2013) supplements.
#' @param x Value of the independent variable
#' @param lambda Scaling parameter
#' @return Value of the dependent variable
#' @keywords size distribution lambda discrete Pareto power law
#' @export
#' @examples in progress

ddpareto <- function(x, lambda){
  #require(VGAM)
  x^-lambda/VGAM::zeta(lambda)
}
