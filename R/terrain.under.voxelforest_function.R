#' Add terrain under a voxelforest
#'
#' Function that shifts a voxelforest vertically to add underlying terrain elevation.
#' @param vxf.dt Input data.table which contains a voxel forest created with make.voxelforest function and the specific settings "keep=C("TreeID", "X", "Y")", i.e. each voxel has to contain the unique tree ID and the stem position
#' @param terrain.dt Input data.table which contains X-, Y- and Z-column representing a digital terrain model at 1-m resolution (e.g., can be obtained from a 1-m DTM raster with data.table(ras2xyzdf(dtm.ras)))
#' @return Data.table containing a voxel forest with terrain
#' @keywords voxel forest plot lidar terrain elevation DTM DSM DEM CHM point cloud trees vertical altitude
#' @export
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

terrain.under.voxelforest <- function(vxf.dt, terrain.dt){

  # Convert to data.tables
  require(data.table)
  vxf.dt <- data.table(vxf.dt)
  terrain.dt <- data.table(terrain.dt)

  # Rename the terrain columns
  setnames(terrain.dt, old=names(terrain.dt), new=c("TerrainX", "TerrainY", "TerrainZ"))

  # Separate tree voxels
  vxf1.dt <- subset(vxf.dt, !is.na(TreeID))
  # Round the stem coordinates to 1-m precision
  vxf1.dt[, round.Xstem := round(Xstem)]
  vxf1.dt[, round.Ystem := round(Ystem)]
  # Merge the terrain height at stem position to each tree voxel
  vxf1.dt <- merge(vxf1.dt, terrain.dt, by.x=c("round.Xstem", "round.Ystem"), by.y=c("TerrainX", "TerrainY"), all.x=T)
  # Shift the tree vertically
  vxf1.dt[, Z := Z+TerrainZ]
  # Keep only relevant colums
  vxf1.dt <- subset(vxf1.dt, select=c("X", "Y", "Z", "LAD", "Xstem", "Ystem", "TreeID", "LAI"))

  # Separate ground voxels
  vxf2.dt <- subset(vxf.dt, is.na(TreeID))
  # Round the coordinates to 1-m precision
  vxf2.dt[, round.X := round(X)]
  vxf2.dt[, round.Y := round(Y)]
  # Merge the terrain height to each ground voxel
  vxf2.dt <- merge(vxf2.dt, terrain.dt, by.x=c("round.X", "round.Y"), by.y=c("TerrainX", "TerrainY"), all.x=T)
  # Shift the ground vertically
  vxf2.dt[, Z := Z+TerrainZ]
  # Keep only relevant colums
  vxf2.dt <- subset(vxf2.dt, select=c("X", "Y", "Z", "LAD", "Xstem", "Ystem", "TreeID", "LAI"))

  # Reunite tree and ground voxels
  out.vxf.dt <- rbind(vxf1.dt, vxf2.dt)
  #nrow(out.vxf.dt)

  # Remove tree voxels which are underground
  setnames(vxf2.dt, old=c("Z"), new=c("TerrainZ"))
  out.vxf.dt <- merge(out.vxf.dt, vxf2.dt[, 1:3], by.x=c("X", "Y"), by.y=c("X", "Y"), all.x=T)
  out.vxf.dt[is.na(TerrainZ), TerrainZ := -Inf]
  out.vxf.dt <- subset(out.vxf.dt, Z >= TerrainZ)
  #nrow(out.vxf.dt)

  #display.point.cloud.dt(out.vxf.dt)

  # Return result
  return(out.vxf.dt)
}


























