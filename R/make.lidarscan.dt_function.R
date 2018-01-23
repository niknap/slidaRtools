#' Scan a voxel forest with a virtual Lidar
#'
#' Function that simulates a Lidar point cloud of a given voxel forest and writes it to a file in XYZ-table format.
#' The model uses a Beer-Lambert light extinction approach and interprets it as probability
#' to get a Lidar return from a voxel.
#' @param vxf.dt data.table containing the voxel forest (derived from make.voxelforest.dt function)
#' @param P0.AGR Initial return probability for above ground voxels
#' @param k.AGR Extinction coefficient for above ground voxels
#' @param P0.GR Initial return probability for ground voxels
#' @param k.GR Extinction coefficient for ground voxels
#' @param LAD.prefactor Boolean to specify whether leaf area density
#' (LAD) of a voxel should be multiplied with P0 to get final return
#' probability or not
#' @param prob.out Boolean to specify whether an additional output containing the
#' full voxelforest with information on canopy above and Lidar return
#' probability for each voxel should be produced
#' @return data.table object containing a XYZ-table of discrete Lidar returns (point cloud).
#' If prob.out=T the function will return a list containing two data.tables: 1) the Lidar
#' point cloud and 2) the voxel forest with Lidar return probability info.
#' @keywords voxel forest plot stand lidar point cloud xyz
#' @export
#' @examples in progress

make.lidarscan.dt <- function(vxf.dt, P0.AGR=0.2, k.AGR=0.2, P0.GR=0.2, k.GR=0.2, LAD.prefactor=F, prob.out=F){
  require(data.table)

  # Convert voxelforest to data.table
  voxelforest.dt <- data.table(vxf.dt)

  # Sort the dataframe in the order X-, Y- and Z-coordinate
  # with the crown layers in decreasing order
  setorderv(voxelforest.dt, c("X", "Y", "Z"), c(1, 1, -1))

  # Calculate cumulative leaf area index for each voxel
  voxelforest.dt[, LAI := cumsum(LAD), by=c("X", "Y")]

  # Calculate Lidar return probability index for each above ground
  # voxel based on LAI using Beer-Lambert's light extinction model
  voxelforest.dt[Z > 0, ReturnProbability := P0.AGR*exp(-k.AGR*LAI)]

  # Calculate Lidar return probability index for each ground
  # voxel based on LAI using Beer-Lambert's light extinction model
  voxelforest.dt[Z == 0, ReturnProbability := P0.GR*exp(-k.GR*LAI)]

  # If the LAD prefactor option is selected multiply the return probability in
  # each voxel with the leaf area density of the voxel to account for the fact
  # that dense foliage reflects the laser more likely than sparse foliage
  if(LAD.prefactor == T){
    voxelforest.dt[, ReturnProbability := LAD*ReturnProbability]
  }

  # # testing
  #   df <- data.frame(voxelforest.dt)
  #   nrow(df)
  #   display.point.cloud(df, size=3, col.lim=c(0, 0.2), col.var="ReturnProbability")

  # Draw a random number between 0 and 1 from a uniform distribution
  # for each voxel
  random.vec <- runif(n=nrow(voxelforest.dt), min=0, max=1)

  # Decide based on return probability and random vector which
  # voxels will contain Lidar returns
  lidar.dt <- subset(voxelforest.dt, ReturnProbability > random.vec)
  nrow(lidar.dt)

  # Number the returns in each vertical voxel column
  lidar.dt[, ReturnNumber := 1:.N, by=c("X", "Y")]

  #   # testing
  #   df <- data.frame(lidar.dt)
  #   nrow(df)
  #   display.point.cloud(df, size=3, col.lim=c(0, 0.2), col.var="ReturnProbability")
  #
  #   df2 <-  thin.point.cloud(df)
  #   display.point.cloud(df2, size=3, col.lim=c(0, 0.2), col.var="ReturnProbability")
  #   display.point.cloud(df2, size=3, col.lim=c(0, 55), col.var="Z")
  #   display.point.cloud(df2, size=3, col.lim=c(0, 7), col.var="ReturnNumber")
  #
  #   df3 <- subset(df2, ReturnNumber < 4)
  #   display.point.cloud(df3, size=3, col.lim=c(0, 7), col.var="ReturnNumber")
  #
  #   hist(df$ReturnNumber)
  #   hist(df2$ReturnNumber)
  #   hist(df3$ReturnNumber)

  # If the prob.out option is off return only the Lidar point cloud as data.table
  if(prob.out == F){
    return(lidar.dt)
  } else {
    # If the prob.out option is on return a list with the Lidar point cloud data.table
    # and the voxelforest data.table with the full information on canopy above and
    # Lidar return probabilities
    result.lst <- list(lidar.dt, voxelforest.dt)
  }
}



