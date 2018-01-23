#' Calculate the Shannon index of a profile
#'
#' Function that returns the Shannon diversity index of a vector
#' of counts or relative proportions (e.g. Lidar profile).
#' @param vec Vector of counts or relative proportions
#' @param ex.vec Vector indices of elements in vec that should be excluded 
#' (e.g. to exclude ground return of a Lidar profile set to 1)
#' @return Shannon index
#' @keywords height profile Lidar Shannon index diversity heterogeneity
#' @export
#' @examples in progress

calc.ShannonIndex <- function(vec, ex.vec=NA){
  # Exclude certain elements
  if(!is.na(ex.vec)){
    vec <- vec[-ex.vec]
  }
  # Exclude classes with value 0
  no.zero.vec <- vec[vec != 0]
  # Calculate the relative contribution of each class
  rel.vec <- no.zero.vec/sum(no.zero.vec, na.rm=T)  
  # Calculate the Shannon index of the vector and normalize with ln of the whole vector length
  (ShannonIndex <- -sum(rel.vec*log(rel.vec), na.rm=T)/log(length(vec)))
  return(ShannonIndex)
}
