################################################################################
# Demonstration workflow for simulating a lidar point cloud (Knapp et al. 2018)
# from a given forest inventory table using the implementation in the
# slidaRtools package
# Contact: nikolai.knapp@ufz.de
################################################################################

#############
# Preparation
#############

# Load required packages
.libPaths()
require(slidaRtools)

# Read and inspect example data
require(repmis)
source_data("https://github.com/niknap/slidaRtools/blob/master/DemoWorkflow/Demo_Data_1ha_Traunstein.rda?raw=True")
head(lid.dt)
head(inv.dt)
nrow(lid.dt)
nrow(inv.dt)

# Check stem map
plot(inv.dt$Y ~ inv.dt$X)

# Prepare the tree attributes which are needed for the simulation
# Rename stem diameter column
inv.dt[, D := DBH]
# Calculate tree height according to Michaelis-Menten function
inv.dt[, H := (60*D)/(0.5+D)]
# Calculate crown length
inv.dt[, CL := 0.4*H]
# Calculate crown diameter according to power law function
inv.dt[, CD := 15*D^0.8]
# Set crown shape to icecone (see documentation for other shapes)
inv.dt[, CS := 4]
# Set leaf area density within the crown (m2/m3)
inv.dt[, LAD := 0.3]


########################################
# 1. Basic lidar simulation workflow
########################################

#############################
# Voxelforest construction
#############################

# Simple version (recommended for areas of a few hectares only)
vxf.dt <- make.voxelforest.dt(inv.dt, minx=0, maxx=100, miny=0, maxy=100)
display.point.cloud.dt(vxf.dt)
head(vxf.dt)

# Parallel version (recommended for areas > 10 ha; res specifies the resolution
# of subareas which are processed in parallel)
vxf2.dt <- make.voxelforest.dt.parallel(inv.dt, minx=0, maxx=100, miny=0,
                                        maxy=100, res=50)
display.point.cloud.dt(vxf2.dt)
head(vxf2.dt)


###############################
# Lidar point cloud simulation
###############################

# Simulate a lidar point cloud from the voxelforest
lid.dt <- make.lidarscan.dt(vxf.dt, P0.AGR = 0.2, k.AGR = 0.2, P0.GR = 0.2)
display.point.cloud.dt(lid.dt)
head(lid.dt)

# Aggregate to vertical profile
prof.vec <- make.profile.from.XYZ(lid.dt)
display.profile(prof.vec, line.col="green")


#############################
# Lidar waveform simulation
#############################

# GEDI footprint size
wf.gedi.vec <- make.large.footprint.lidar.pulse(vxf.dt, Xctr=50, Yctr=50,
                                                diameter=25, k=0.2,
                                                sd=0.25*25, VG.ratio=1)
display.profile(wf.gedi.vec, line.col="red", xlab="Relative energy")

# ICESat GLAS footprint size
wf.icesat.vec <- make.large.footprint.lidar.pulse(vxf.dt, Xctr=50, Yctr=50,
                                                  diameter=70, k=0.2,
                                                  sd=0.25*70, VG.ratio=1)
display.profile(wf.icesat.vec, add=T, line.col="blue")

# Add the normalized simulated ALS profile to the plot
norm.prof.vec <- prof.vec/sum(prof.vec)
display.profile(norm.prof.vec, add=T, line.col="green")


###########################
# 2. Additional options
###########################

################################################################
# Keep tree attributes in the voxelforest and lidar point cloud
################################################################

# The keep option allows to add any of the information from the tree table to
# each voxel in the voxelforest, e.g., the TreeID. The X and Y coordinates of
# the tree are renamed to Xstem and Ystem, because X and Y are the voxel
# coordinates in the voxelforest
vxf.dt <- make.voxelforest.dt(inv.dt, minx=0, maxx=100, miny=0, maxy=100,
                              keep=c("X", "Y", "TreeID"))
display.point.cloud.dt(vxf.dt)
head(vxf.dt)

# Visualize with TreeID coloring
max.TreeID <- max(inv.dt$TreeID)
display.point.cloud.dt(vxf.dt, col.var="TreeID",
                       col.palette=rainbow(max.TreeID)[sample(max.TreeID)],
                       col.lim=c(0, max.TreeID), size=2)

# Simulate a lidar point cloud
lid.dt <- make.lidarscan.dt(vxf.dt, P0.AGR = 0.2, k.AGR = 0.2, P0.GR = 0.2)
head(lid.dt)

# Visualize with TreeID coloring
display.point.cloud.dt(lid.dt, col.var="TreeID",
                       col.palette=rainbow(max.TreeID)[sample(max.TreeID)],
                       col.lim=c(0, max.TreeID), size=2)


############################################
# Make periodic boundaries at plot borders
############################################

# Let tree crowns that exceed the plot at one side reappear from the
# opposite side
vxf.dt <- periodic.boundaries.dt(vxf.dt, minx=0, maxx=100, miny=0, maxy=100)
display.point.cloud.dt(vxf.dt)

# Visualize with TreeID coloring
max.TreeID <- max(inv.dt$TreeID)
display.point.cloud.dt(vxf.dt, col.var="TreeID",
                       col.palette=rainbow(max.TreeID)[sample(max.TreeID)],
                       col.lim=c(0, max.TreeID), size=2)


#############################
# Add terrain below forest
#############################

# Simulate a simple terrain based on cosinus fuctions in X and Y direction
terrain.dt <- data.table(expand.grid(X=0:100, Y=0:100, Z=NA))
terrain.dt[, Z := 4*(cos(X/10)+cos(Y/10))+50]
display.point.cloud.dt(terrain.dt)
terrain.ras <- raster.from.point.cloud(terrain.dt)
plot(terrain.ras)

# Put the voxelforest on the terrain
vxf.terrain.dt <- terrain.under.voxelforest(vxf.dt, terrain.dt)
display.point.cloud.dt(vxf.terrain.dt, size=2)

# Simulate lidar point cloud
lid.dt <- make.lidarscan.dt(vxf.terrain.dt)
display.point.cloud.dt(lid.dt, size=2)

# Compare DSMs on flat and hilly terrain
dsm.flat.ras <- raster.from.point.cloud(vxf.dt, res=1)
dsm.hilly.ras <- raster.from.point.cloud(vxf.terrain.dt, res=1)
plot(dsm.flat.ras)
plot(dsm.hilly.ras)



