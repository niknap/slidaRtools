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



#' Calculate scaling parameter lambda of a discrete power law distribution
#'
#' Function that returns the scaling parameter lambda of a
#' discrete Pareto (power law) distribution (e.g. canopy gap sizes,
#' tree diameters) using Maximum-Likelihood estimation (MLE);
#' Code adapted from Asner et al. (2013) supplements.
#' @param vec vector of (discrete) size measurements
#' @return vector of two elements: [1] lambda, [2] two-times the negative log-likelihood of the ML-fit
#' @keywords size distribution lambda discrete Pareto power law
#' @export
#' @examples in progress

calc.discrete.Pareto.lambda <- function(vec){
  # Function for the density of the discrete Pareto distribution with
  # scaling parameter lambda
  ddpareto <- function(x, lambda){
    #require(VGAM)
    x^-lambda/VGAM::zeta(lambda)
  }
  # Function to return negative 2 times the log-likelihood of data under a
  # discrete Pareto distribution with scaling parameter lambda.
  dplik <- function(data, lambda){
    2*sum(-log(ddpareto(x=data, lambda=lambda)))
  }
  # Estimate (MLE) the scaling parameter lambda of the size distribution,
  # assuming a discrete Pareto distribution (discrete power law distribution).
  # If maximum=F the algorithm searches for the minimum, if T for the maximum.
  fit <- optimize(dplik, data=vec, lower=1.0001, upper=20, maximum=F)
  # best fit for lambda
  fit$minimum
  # associated -2LL
  fit$objective
  result <- c(fit$minimum, fit$objective)
  names(result) <- c("lambda", "-2LL")
  return(result)
}







