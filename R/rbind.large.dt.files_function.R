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





