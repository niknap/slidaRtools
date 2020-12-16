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



#' A function to calculate the CSI cummulative Size Index
#'
#' A function to calculate the CSI cummulative Size Index
#' @param dbh, vector of all dbh of a plot in m
#' @param h_max, height of the talles tree in the plot
#' @param Xfactor, which Fraction of the cummulative dbh should be devided by the cummulative dbh
#' @param part2, "no" returns only part 1, "dbh_max" multilpies part1 with dbhmax^2.5; "h_max" multiplies part 1 with f(h_max,dbh_max);"old" with h_max^1.5
#' @return dimensionless CSI
#' @keywords cumulative size index
#' @export
#' @examples in progress
#' @author Friedrich J. Bohn

calc.cumulative.size.index <- function(dbh, h_max, Xfactor=0.6, part2="old"){

  dbh<-sort(dbh)
  cumDbh<-cumsum(dbh)
  Xanteil<-max(cumDbh)*Xfactor
  # can be improved:
  Xdbh<-which.min(abs(cumDbh-Xanteil))

  part1<-dbh[Xdbh]/max(dbh)

  if(part2=="no"){
    return(part1)
  }else if(part2=="h_max"){
    return(part1*(max(dbh)*h_max)^1.25)
  }else if(part2=="dbh_max"){
    return(part1*max(dbh)^2.5)
  }else if(part2=="old"){
    return(part1*h_max^1.5)
  }
}

