#' Convert coordinates from degrees, minutes and seconds to decimal degrees
#'
#' Function that converts between coordinate notations
#' @param deg Degrees
#' @param min Minutes
#' @param sec Seconds
#' @return Decimal degrees
#' @keywords decimal degree coordinate minute second
#' @export
#' @examples deg.min.sec.to.dec.deg(5, 4, 3)

deg.min.sec.to.dec.deg <- function(deg, min, sec){
  return(deg+(60*min+sec)/3600)
}
