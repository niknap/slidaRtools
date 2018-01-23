#' Scan a voxel forest with a virtual large footprint Lidar
#'
#' Function that simulates a Lidar waveform of a given voxel forest and returns a profile vector.
#' The horizontal energy distribution inside the pulse is simulated with a Gaussian distribution.
#' @param vxf voxel forest created with make.voxelforest function
#' @param Xctr X-coordinate of the pulse center
#' @param Yctr Y-coordinate of the pulse center
#' @param diameter Diameter of the pulse footprint in meters (e.g. LVIS = 25 m, Icesat GLAS = 70 m)
#' @param binwidth Vertical resolution of the waveform in meters
#' @param k Extinction coefficient
#' @param sd Standard deviation of the Gaussian energy distribution (for a curve that approaches 0 at the margin of the pulse use sd = 0.4*radius)
#' @param VG.ratio Reflectance ratio between vegetation surface and ground surface
#' @return Vector of the Lidar waveform (normalized to a sum of 1)
#' @keywords voxel forest plot lidar large footprint pulse waveform profile gaussian
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

make.large.footprint.lidar.pulse <- function(vxf, Xctr, Yctr, diameter=25, binwidth=1, k=0.2, sd=0.4, VG.ratio=2.5){
  vxf.dt <- data.table(vxf)
  # Cut out the voxels that fall into the pulse cylinder
  sub.dt <- vxf.dt[in.circle(vxf.dt$X, vxf.dt$Y, Xctr, Yctr, diameter/2) == T]
  # Calculate relative intensity for each voxel using Beer-Lambert light extinction.
  # Absolute value is irrelevant, because final profile will be normalized to a sum of 1.
  sub.dt[, I := exp(-k*LAI)]
  # Adjust the intensity at ground level, considering the vegetation vs. ground refectance ratio
  sub.dt[Z == 0, I := I/VG.ratio]
  # Weight voxels in the pulse center higher than at the margins using a 2D Gaussian weighting function.
  sub.dt$weight <- calc.gaussian.function.2D(Xpt=sub.dt$X, Ypt=sub.dt$Y, amp=1, Xctr=Xctr, Yctr=Yctr, sd=sd)
  # Multiply each voxel's intensity with its weight
  sub.dt[, I := I*weight]
  # Aggregate the XYZ-table into a vertical profile vector
  sub.dt <- subset(sub.dt, select=c("X", "Y", "Z", "I"))
  sub.df <- data.frame(sub.dt)
  prof <- make.profile.from.XYZ.value(sub.df, binwidth, stat="sum")
  # Normalize the profile
  prof <- prof/sum(prof)
  return(prof)
}







