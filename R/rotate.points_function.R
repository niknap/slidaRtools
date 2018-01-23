#' Rotate points around X-, Y- or Z-axis
#'
#' Function to rotate a given set of points or point cloud in a
#' plain around center point with a certain angle.
#' @param cor.df Coordinate dataframe with columns containing X-, Y- and Z-coordinate (at least
#' two columns are required and names need to be "X", "Y" or "Z", respectively)
#' @param rot.angle Rotation angle in degrees (clockwise rotation)
#' @param rot.axis Rotation axis (default is "Z", can also be set to "X" or "Y")
#' @param center Vector of three elements specifying the  X-, Y- and Z-coordinate of the rotation center point
#' @return Coordinate dataframe with X- and Y-coordinates after rotation
#' @keywords point cloud rotation angle transform
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

rotate.points <- function(cor.df, rot.angle, rot.axis="Z", center=c(0, 0, 0)){
  cor.df <- data.frame(cor.df)
  # Convert degrees to radians
  rot.angle <- -rot.angle*(0.5*pi)/90
  if(rot.axis == "Z"){
    # Rotation around Z axis
    relx <- cor.df$X - center[1]
    rely <- cor.df$Y - center[2]
    # Calculate new absolute coordinates by adding the center coordinates
    xout <- relx*cos(rot.angle) - rely*sin(rot.angle) + center[1]
    yout <- rely*cos(rot.angle) + relx*sin(rot.angle) + center[2]
    # Replace the X- and Y-coordinates and return rotated dataframe as output
    rot.df <- cor.df
    rot.df$X <- xout
    rot.df$Y <- yout
  } else if(rot.axis == "Y"){
    # Rotation around Y axis
    relx <- cor.df$X - center[1]
    relz <- cor.df$Z - center[3]
    # Calculate new absolute coordinates by adding the center coordinates
    xout <- relx*cos(rot.angle) - relz*sin(rot.angle) + center[1]
    zout <- relz*cos(rot.angle) + relx*sin(rot.angle) + center[3]
    # Replace the X- and Y-coordinates and return rotated dataframe as output
    rot.df <- cor.df
    rot.df$X <- xout
    rot.df$Z <- zout
  } else if(rot.axis == "X"){
    # Rotation around X axis
    rely <- cor.df$Y - center[2]
    relz <- cor.df$Z - center[3]
    # Calculate new absolute coordinates by adding the center coordinates
    yout <- rely*cos(rot.angle) - relz*sin(rot.angle) + center[2]
    zout <- relz*cos(rot.angle) + rely*sin(rot.angle) + center[3]
    # Replace the X- and Y-coordinates and return rotated dataframe as output
    rot.df <- cor.df
    rot.df$Y <- yout
    rot.df$Z <- zout
  }
  return(rot.df)
}

