#' Point cloud thinning
#'
#' Function that thins a point cloud to a desired homogenous
#' horizontal point density by keeping a random subset of points
#' of a desired size in each spatial cell and discarding all other points.
#' @param pc Point cloud in XYZ-table format (important: column names need to be X, Y and Z)
#' @param cell.size Side length of a spatial unit
#' @param returns.per.cell Number of returns that should be kept in each cell
#' @return data.frame of the thinned point cloud in XYZ-table format
#' @keywords thinning subset random point cloud density subsample
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

thin.point.cloud <- function(pc, cell.size=1, returns.per.cell=4){

  # Convert input to a data.frame
  pc <- as.data.frame(pc)

  # Make the sampling grid matrix
  minx <- min(pc$X)
  miny <- min(pc$Y)
  maxx <- max(pc$X)
  maxy <- max(pc$Y)
  maxx.gridcell <- ceiling((maxx-minx)/cell.size)
  maxy.gridcell <- ceiling((maxy-miny)/cell.size)
  # Calculate the cell number for each point
  pc$CellNo <- calc.spatial.index(pc$X, pc$Y, cell.size)
  # Count how many returns fall into each cell
  returns.per.cell.count <- table(pc$CellNo)
  # Split the point cloud into cells with many returns (> target density)
  # and with few returns (<= target density)
  returns.bigcount <- returns.per.cell.count[returns.per.cell.count > returns.per.cell]
  returns.many <- pc[pc[, "CellNo"] %in% names(returns.bigcount), ]
  nrow(returns.many)
  head(returns.many)
  returns.few <- pc[!pc[, "CellNo"] %in% names(returns.bigcount), ]
  nrow(returns.few)
  # Thinning is only required for cells with many returns and is done via
  # random subsampling
  if(nrow(returns.many) > 0){
    thinning.function <- function(inputvector){
      sample(inputvector, size=returns.per.cell, replace = FALSE, prob = NULL)
    }
    return.ID <- 1:nrow(returns.many)
    sampled.IDs <- unlist(tapply(return.ID, returns.many$CellNo, thinning.function))
    returns.many.thinned <- returns.many[return.ID %in% sampled.IDs, ]
    nrow(returns.many.thinned)
    head(returns.many.thinned)
    # Recombine the split data
    pc.out <- rbind(returns.few, returns.many.thinned)
  } else {
    pc.out <- returns.few
  }
  # Remove the cell number column
  pc.out <- subset(pc.out, select=-CellNo)
  return(pc.out)
}
