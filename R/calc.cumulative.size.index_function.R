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

