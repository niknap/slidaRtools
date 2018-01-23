#' Convert coordinates from decimal degrees to degrees, minutes and seconds
#'
#' Function that converts between coordinate notations
#' @param dd Decimal degrees
#' @return Degrees, minutes and seconds
#' @keywords decimal degree coordinate minute second
#' @export
#' @examples dec.deg.to.deg.min.sec(10.51)

dec.deg.to.deg.min.sec <- function(dd){
  d <- floor(dd)
  m <- floor((dd - d) * 60)
  s <- (dd - d - m/60) * 3600
  result <- c(d, m, s)
  names(result) <- c("deg", "min", "sec")
  return(result)
}
