#' Read large tables from files and rbind them into one huge data table
#'
#' Function that reads a group of equally structured tables from files
#' (e.g. point clouds in XYZ-format) and integrates them into one huge
#' data table
#' @param path ...to the directory with the input files
#' @param pattern ...that is containded in all input filenames
#' @return data table containing all rows of all input tables
#' @keywords large big data table XYZ file list rbind
#' @export
#' @examples in progress

rbind.large.dt.files <- function(path, pattern){
  filelist <- list.files(path=path, pattern=pattern)
  dt <- rbindlist(lapply(filelist, fread))
  return(dt)
}





