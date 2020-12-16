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



#' Find extrema
#'
#' Function that finds local extrema in curves like canopy
#' height profiles with a moving window.
#' @param
#' @return
#' @keywords
#' @export
#' @examples in progress

extrema.finder <- function(curve, wr, extr){
  clen <- length(curve)
  new.curve <- vector(length=clen+2*wr)
  new.curve[1:wr] <- curve[1]
  new.curve[(wr+1):(wr+clen)] <- curve
  new.curve[(wr+clen+1):(wr+clen+wr)] <- curve[clen]
  extrema <- rep(F, times=clen+2*wr)
  if(extr == "min"){
    for(i in (wr+1):(wr+clen)){
      if(new.curve[i] == min(new.curve[(i-wr): (i+wr)])){
        extrema[i] <- T
      }
    }
  } else if(extr == "max"){
    for(i in (wr+1):(wr+clen)){
      if(new.curve[i] == max(new.curve[(i-wr): (i+wr)])){
        extrema[i] <- T
      }
    }
  }
  extrema <- extrema[(wr+1):(wr+clen)]
  names(extrema) <- names(curve)
  extrema[1] <- F
  extrema[clen] <- F
  return(extrema)
}

