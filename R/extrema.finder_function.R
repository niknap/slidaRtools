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

