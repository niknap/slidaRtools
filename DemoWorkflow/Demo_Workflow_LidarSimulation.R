################################################################################
# Demonstration workflow for simulating a lidar point cloud from a given forest
# inventory table using the implementation in the slidaRtools package
# Contact: nikolai.knapp@thuenen.de
#
# References:
#
# Knapp, N., Fischer, R., & Huth, A. (2018). Linking lidar and forest modeling
# to assess biomass estimation across scales and disturbance states.
# Remote Sensing of Environment, 205, 199-209.
#
# Knapp, N., Huth, A., & Fischer, R. (2021). Tree crowns cause border effects
# in area-based biomass estimations from remote sensing.
# Remote Sensing, 13(8), 1592.
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
vxf.dt <- make_voxelforest(inv.dt, minx=0, maxx=100, miny=0, maxy=100)
display_point_cloud(vxf.dt)
head(vxf.dt)

# Parallel version (recommended for areas > 10 ha; res specifies the resolution
# of subareas which are processed in parallel)
vxf2.dt <- make_voxelforest_parallel(inv.dt, minx=0, maxx=100, miny=0,
                                     maxy=100, res=50)
display_point_cloud(vxf2.dt)
head(vxf2.dt)


###############################
# Lidar point cloud simulation
###############################

# Simulate a lidar point cloud from the voxelforest
sim.lid.dt <- make_lidarscan(vxf.dt, P0.AGR=0.2, P0.GR=0.2, k=0.2)
display_point_cloud(sim.lid.dt)
head(sim.lid.dt)

# Aggregate to vertical profile
sim.lid.prof.vec <- make_profile_from_XYZ(sim.lid.dt)
display_profile(sim.lid.prof.vec, line.col="blue")


###############################
# Leaf area density profiles
###############################

# Derive leaf area density profile for the voxelforest
sel.col <- c("X", "Y", "Z", "LAD")
xyz.lad.dt <- vxf.dt[, ..sel.col]
lad.prof.vec <- make_profile_from_XYZ_value(xyz.lad.dt, stat="sum")
# Set ground value to zero, because ground area is not leaf area
lad.prof.vec[1] <- 0
# Divide by the area (here 1 ha) to get to m2/m3 units
lad.prof.vec <- lad.prof.vec / 10000
display_profile(lad.prof.vec, line.col="green",
                xlab=bquote("Leaf area density [m"^2 ~ "m"^-3 ~"]"))


#############################
# Lidar waveform simulation
#############################

# GEDI footprint size
wf.gedi.vec <- make_large_footprint_lidar_pulse(vxf.dt, Xctr=50, Yctr=50,
                                                diameter=25, k=0.2,
                                                sd=0.25*25, VG.ratio=1)
display_profile(wf.gedi.vec, line.col="red", xlab="Relative energy")

# ICESat GLAS footprint size
wf.icesat.vec <- make_large_footprint_lidar_pulse(vxf.dt, Xctr=50, Yctr=50,
                                                  diameter=70, k=0.2,
                                                  sd=0.25*70, VG.ratio=1)
display_profile(wf.icesat.vec, add=T, line.col="orange")

# Add the normalized simulated ALS profile to the plot
norm.sim.lid.prof.vec <- sim.lid.prof.vec/sum(sim.lid.prof.vec)
display_profile(norm.sim.lid.prof.vec, add=T, line.col="blue")

# Add the normalized leaf area density profile to the plot
norm.lad.prof.vec<- lad.prof.vec/sum(lad.prof.vec)
display_profile(norm.lad.prof.vec, add=T, line.col="green")


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
vxf.dt <- make_voxelforest(inv.dt, minx=0, maxx=100, miny=0, maxy=100,
                           keep=c("X", "Y", "TreeID"))
display_point_cloud(vxf.dt)
head(vxf.dt)

# Visualize with TreeID coloring
max.TreeID <- max(inv.dt$TreeID)
display_point_cloud(vxf.dt, col.var="TreeID",
                    col.palette=rainbow(max.TreeID)[sample(max.TreeID)],
                    col.lim=c(0, max.TreeID), size=2)

# Simulate a lidar point cloud
sim.lid.dt <- make_lidarscan(vxf.dt, P0.AGR=0.2, P0.GR=0.2, k=0.2)
head(sim.lid.dt)

# Visualize with TreeID coloring
display_point_cloud(sim.lid.dt, col.var="TreeID",
                    col.palette=rainbow(max.TreeID)[sample(max.TreeID)],
                    col.lim=c(0, max.TreeID), size=2)


############################################
# Make periodic boundaries at plot borders
############################################

# Let tree crowns that exceed the plot at one side reappear from the
# opposite side
vxf.dt <- periodic_boundaries(vxf.dt, minx=0, maxx=100, miny=0, maxy=100)
display_point_cloud(vxf.dt)

# Visualize with TreeID coloring
max.TreeID <- max(inv.dt$TreeID)
display_point_cloud(vxf.dt, col.var="TreeID",
                    col.palette=rainbow(max.TreeID)[sample(max.TreeID)],
                    col.lim=c(0, max.TreeID), size=2)


#############################
# Add terrain below forest
#############################

# Simulate a simple terrain based on cosinus fuctions in X and Y direction
terrain.dt <- data.table(expand.grid(X=0:100, Y=0:100, Z=NA))
terrain.dt[, Z := 4*(cos(X/10)+cos(Y/10))+50]
display_point_cloud(terrain.dt)
terrain.ras <- raster_from_point_cloud(terrain.dt)
plot(terrain.ras)

# Put the voxelforest on the terrain
vxf.terrain.dt <- terrain_under_voxelforest(vxf.dt, terrain.dt)
display_point_cloud(vxf.terrain.dt, size=2)

# Simulate lidar point cloud
sim.lid.dt <- make_lidarscan(vxf.terrain.dt)
display_point_cloud(sim.lid.dt, size=2)

# Compare DSMs on flat and hilly terrain
dsm.flat.ras <- raster_from_point_cloud(vxf.dt, res=1)
dsm.hilly.ras <- raster_from_point_cloud(vxf.terrain.dt, res=1)
plot(dsm.flat.ras)
plot(dsm.hilly.ras)



