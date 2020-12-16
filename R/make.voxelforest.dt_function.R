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



#' Make a voxel forest from a tree list
#'
#' Function that creates a voxel forest and writes it to a file in XYZ-table format.
#' @param trees.dt A data.table (or data.frame) that contains all trees that form the voxelforest provided
#' with specific requirements: Each row represents one individual. The columns need to
#' have the following names and content: X = X-coordinate of tree position, Y = Y-coordinate
#' of tree position; H = tree height (m), CD = tree crown diameter (m), CL = tree crown length (m),
#' LAD = leaf area density of the tree (m2/m3), CS = tree crown shape (coding: 1 = cylinder,
#' 2 = spheroid, 3 = cone, 4 = icecone)
#' @param minx Minimum X-coordinate of the simulated area
#' @param maxx Maximum X-coordinate of the simulated area
#' @param miny Minimum Y-coordinate of the simulated area
#' @param maxy Maximum Y-coordinate of the simulated area
#' @param vxl.per.sqm Spatial resolution of the simulation in voxels per square meter
#' @param stems Boolean whether to include tree stems in the simulation or not
#' @param ground Boolean whether to include forest ground in the simulation or not
#' @param aggregation.func Function specifying how to deal with voxels in overlapping crowns
#' (that belong to more than one tree): Either the maximum, sum, mean or minimum of all
#' different values for that voxel can be taken, or if set to NA the values do not get
#' aggregated, thus multiple rows for that voxel are kept in the final voxelforest output.
#' Aggregation can only be performed if no additional columns should be kept, to avoid ambiguities.
#' If the parameter keep is set to something other than NA, it has priority and aggregation is
#' suppressed.
#' @param keep Optional vector of column names from which information should be kept
#' in each voxel of the output. E.g. choose keep=c("X", "Y", "TreeID") if stem positions and
#' ID of each tree should be available for each voxel. X and Y are special cases, because in
#' the output the columns "X" and "Y" specify the voxel coordinates. Thus, the columns
#' containing the stem coordinates are renamed to "Xstem" and "Ystem". Other kept columns
#' will appear in the output under their original input name.
#' @return data.table object containing a XYZ-table of tree crown voxels, ground voxels and maybe tree stem voxels
#' @keywords voxel forest plot stand lidar point cloud xyz
#' @import data.table
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.voxelforest.dt <- function(trees.dt, minx, maxx, miny, maxy, vxl.per.sqm=4,
                                stems=F, ground=T, aggregation.func="max", keep=NA){

  #   # Settings for testing
  # minx <- 100
  # maxx <- 200
  # miny <- 0
  # maxy <- 100
  # vxl.per.sqm=1
  # stems=F
  # ground=T
  # aggregation.func="max"
  # trees.dt <- data.table(sub.inv.dt[60,])

  # Convert treelist to data.table
  trees.dt <- data.table(trees.dt)
  # Check with is.data.table (necessary for parLapply)
  if(is.data.table(trees.dt) == T){

    # Calculate resolution properties
    vxl.sidelength <- 1/((vxl.per.sqm)^0.5)
    vxl.per.m <- (vxl.per.sqm)^0.5

    # Convert the coordinates of the treelist to fit the desired LiDAR resolution
    trees.dt[, Xres := round(X * vxl.per.m)]
    trees.dt[, Yres := round(Y * vxl.per.m)]
    trees.dt[, CDres := round(CD * vxl.per.m)]
    trees.dt[, CRres := ceiling(CDres/2)]

    # Create an empty data.table to store the voxelized forest
    voxelforest.dt <- data.table()

    # Split the trees into crown shape classes
    for(my.CS in unique(trees.dt$CS)){
      #my.CS <- 1
      sub.trees.dt <- subset(trees.dt, CS == my.CS)
      if(nrow(sub.trees.dt) > 0){

        # Repeat each row in the tree data.table as many times as there are voxels in the
        # bounding box in Z direction
        sub.trees.voxel.dt <- sub.trees.dt[rep(1:.N, ceiling(H+1))]
        # Number the replicates
        sub.trees.voxel.dt[, Zbox := 1:.N, by="TreeID"]

        # Repeat each row in the tree data.table as many times as there are voxels in the
        # bounding box in Y direction
        sub.trees.voxel.dt <- sub.trees.voxel.dt[rep(1:.N, ceiling(CDres+5))]
        # Number the replicates
        sub.trees.voxel.dt[, Ybox := 1:.N, by=c("TreeID", "Zbox")]

        # Repeat each row in the tree data.table as many times as there are voxels in the
        # bounding box in X direction
        sub.trees.voxel.dt <- sub.trees.voxel.dt[rep(1:.N, ceiling(CDres+5))]
        # Number the replicates
        sub.trees.voxel.dt[, Xbox := 1:.N, by=c("TreeID", "Zbox", "Ybox")]

        # Shift the bounding boxes to the respective tree XY-positions
        sub.trees.voxel.dt[, Xbox := Xbox + Xres - CRres - 1]
        sub.trees.voxel.dt[, Ybox := Ybox + Yres - CRres - 1]

        #         # testing
        #         df <- data.frame(sub.trees.voxel.dt)
        #         df <- subset(df, select=c("Xbox", "Ybox", "Zbox"))
        #         names(df) <- c("X", "Y", "Z")
        #         display.point.cloud(df)

        # Experiment with distance dependent LAD
        #       # Calculate the distance to the crown center
        #       sub.trees.voxel.dt$CtrDist <- ((sub.trees.voxel.dt$Xbox-sub.trees.voxel.dt$Xres)^2+(sub.trees.voxel.dt$Ybox-sub.trees.voxel.dt$Yres)^2+(sub.trees.voxel.dt$Zbox-(sub.trees.voxel.dt$H-sub.trees.voxel.dt$CL/2))^2)^(1/3)
        #       sub.trees.voxel.dt$LAD <- sub.trees.voxel.dt$LAD * sub.trees.voxel.dt$CtrDist

        # Depending on the crown shape subset only voxels that fall into the crown
        if(my.CS == 1){
          sub.crown.voxel.dt <- subset(sub.trees.voxel.dt, in.cylinder(Xcor=sub.trees.voxel.dt$Xbox, Ycor=sub.trees.voxel.dt$Ybox, Zcor=sub.trees.voxel.dt$Zbox, Xctr=sub.trees.voxel.dt$Xres, Yctr=sub.trees.voxel.dt$Yres, Zbase=(sub.trees.voxel.dt$H-sub.trees.voxel.dt$CL), radius=sub.trees.voxel.dt$CRres, height=sub.trees.voxel.dt$CL) == 1)
        } else if(my.CS == 2){
          sub.crown.voxel.dt <- subset(sub.trees.voxel.dt, in.spheroid(Xcor=sub.trees.voxel.dt$Xbox, Ycor=sub.trees.voxel.dt$Ybox, Zcor=sub.trees.voxel.dt$Zbox, Xctr=sub.trees.voxel.dt$Xres, Yctr=sub.trees.voxel.dt$Yres, Zctr=(sub.trees.voxel.dt$H-sub.trees.voxel.dt$CL/2), radius=sub.trees.voxel.dt$CRres, height=sub.trees.voxel.dt$CL) == 1)
        } else if(my.CS == 3){
          sub.crown.voxel.dt <- subset(sub.trees.voxel.dt, in.cone(Xcor=sub.trees.voxel.dt$Xbox, Ycor=sub.trees.voxel.dt$Ybox, Zcor=sub.trees.voxel.dt$Zbox, Xctr=sub.trees.voxel.dt$Xres, Yctr=sub.trees.voxel.dt$Yres, Zbase=(sub.trees.voxel.dt$H-sub.trees.voxel.dt$CL), radius=sub.trees.voxel.dt$CRres, height=sub.trees.voxel.dt$CL) == 1)
        } else if(my.CS == 4){
          sub.crown.voxel.dt <- subset(sub.trees.voxel.dt, in.icecone(Xcor=sub.trees.voxel.dt$Xbox, Ycor=sub.trees.voxel.dt$Ybox, Zcor=sub.trees.voxel.dt$Zbox, Xctr=sub.trees.voxel.dt$Xres, Yctr=sub.trees.voxel.dt$Yres, Zctr=(sub.trees.voxel.dt$H-sub.trees.voxel.dt$CL/2), radius=sub.trees.voxel.dt$CRres, height=sub.trees.voxel.dt$CL) == 1)
        }

        # Collect results
        voxelforest.dt <- rbind(voxelforest.dt, sub.crown.voxel.dt)

        # Add tree stems
        if(stems == T){
          sub.stem.voxel.dt <- subset(sub.trees.voxel.dt, Xbox==Xres & Ybox==Yres & Zbox<(H-CL))
          voxelforest.dt <- rbind(voxelforest.dt, sub.stem.voxel.dt)
        }
      }
    }

    # Add the forest floor using CJ which works like expand.grid, but in a fast data.table speed
    if(ground == T){
      ground.voxel.dt <- CJ(Xbox=(minx*vxl.per.m):(maxx*vxl.per.m), Ybox=(miny*vxl.per.m):(maxy*vxl.per.m), Zbox=0, LAD=1)
      voxelforest.dt <- rbind(voxelforest.dt, ground.voxel.dt, fill=T)
    }

    # Check if certain columns should be kept
    if(length(keep) == 1){
      if(is.na(keep)){
        # Subset only the relevant columns
        voxelforest.dt <- subset(voxelforest.dt, select=c("Xbox", "Ybox", "Zbox", "LAD"))
        setnames(voxelforest.dt, c("X", "Y", "Z", "LAD"))
        voxelforest.dt$LAD <- round(voxelforest.dt$LAD, 2)
      }
    } else {
      voxelforest.dt <- subset(voxelforest.dt, select=c("Xbox", "Ybox", "Zbox", "LAD", keep))
      keep[which(keep == "X")] <- "Xstem"
      keep[which(keep == "Y")] <- "Ystem"
      setnames(voxelforest.dt, c("X", "Y", "Z", "LAD", keep))
      voxelforest.dt$LAD <- round(voxelforest.dt$LAD, 2)
    }

    # Aggregate redundant points (i.e. voxels in crown overlaps).
    # The syntax from the data.table package takes the statistic from the column "value", grouped by unique
    # combinations of columns "X", "Y" and "Z" and writes it to a column "value" in the output.
    # Aggregation can only be performed if no additional columns should be kept, to avoid ambiguities.
    if(length(keep) == 1){
      if(is.na(keep)){
        if(aggregation.func == "max"){
          voxelforest.dt <- voxelforest.dt[,.(LAD=max(LAD)), by='X,Y,Z']
        } else if(aggregation.func == "mean"){
          voxelforest.dt <- voxelforest.dt[,.(LAD=mean(LAD)), by='X,Y,Z']
        } else if(aggregation.func == "min"){
          voxelforest.dt <- voxelforest.dt[,.(LAD=min(LAD)), by='X,Y,Z']
        } else if(aggregation.func == "sum"){
          voxelforest.dt <- voxelforest.dt[,.(LAD=sum(LAD)), by='X,Y,Z']
        }
      }
    }

    # Convert LiDAR density skewed coordinates to real coordinates in meters
    voxelforest.dt[, X := round(X * vxl.sidelength, 2)]
    voxelforest.dt[, Y := round(Y * vxl.sidelength, 2)]

    # Sort the dataframe in the order X-, Y- and Z-coordinate
    # with the crown layers in decreasing order
    setorderv(voxelforest.dt, c("X", "Y", "Z"), c(1, 1, -1))

    # Calculate cumulative leaf area index for each voxel
    voxelforest.dt[, LAI := cumsum(LAD), by=c("X", "Y")]

    return(voxelforest.dt)
  }
}





