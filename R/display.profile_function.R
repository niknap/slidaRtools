#' Plot vertical profile, e.g. of a Lidar dataset
#'
#' Function for profile plotting.
#' @param prof.vec Profile with values and height bins as names (derived from make.profile functions)
#' @param line.col Line color
#' @param fill.col Polygon fill color (set to NA if no fill is desired)
#' @param add If F a new graph is drawn, if T the profile is plotted into an existing graph
#' @param maxx X-axis maximum
#' @param maxy Y-axis maximum
#' @param alpha.line Transparency setting for the line
#' @param alpha.fill Transparency setting for the polygon fill
#' @param lwd Line width
#' @param lty Line type
#' @param GR.mark Character that should be displayed at the tip of the ground return peak (if none set to NA)
#' @param GR.mark.cex Size of the ground return marker
#' @param GR.mark.lwd Line width of the ground return marker
#' @param GR.mark.col Color of the ground return marker
#' @param alpha.GR.mark Transparency setting for the ground return marker
#' @param xlab X-axis titel
#' @param ylab Y-axis titel
#' @param cex.axis Font size of axis numbers
#' @param cex.lab Font size of axis titels
#' @param las Orientation of axis numbers
#' @return Graphic of the profile
#' @keywords profile 2D plot display point cloud graphics visualization lidar
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

display.profile <- function(prof.vec, line.col="black", fill.col=NA, add=F, maxx=NA, maxy=NA,
                            alpha.line=1, alpha.fill=0.5, lwd=2, lty=1, GR.mark=NA,
                            GR.mark.cex=1, GR.mark.lwd=2, GR.mark.col=NA, alpha.GR.mark=1,
                            xlab=bquote("Count of Lidar returns [ha"^ -1 ~ "]"),
                            ylab="Height [m]",
                            cex.axis=1,
                            cex.lab=1, las=1){
  require(grDevices)
  # Adjust the axes maxima
  h.vec <- as.numeric(as.character(names(prof.vec)))
  if(is.na(maxx)){
    maxx <- 1.2*max(prof.vec)
  }
  if(is.na(maxy)){
    maxy <- 1.2*max(h.vec)
  }
  # Draw the coordinate system frame
  if(add==F){
    plot(c(0, h.vec) ~ c(0, prof.vec),
         type="n", xlab=xlab,
         ylab=ylab, xlim=c(0, maxx), ylim=c(0, maxy),
         cex.axis=cex.axis, cex.lab=cex.lab, las=las)
  }
  # Add a filled polygon
  if(!is.na(fill.col)){
    polygon(x=c(0, prof.vec, 0), y=c(0, h.vec, h.vec[length(h.vec)]), col=adjustcolor(fill.col, alpha.fill), border=F)
  }
  # Add the profile line
  points(x=c(0, prof.vec, 0), y=c(0, h.vec, h.vec[length(h.vec)]), type="l",
         col=adjustcolor(line.col, alpha.line), lwd=lwd, lty=lty)
  # Add a ground return mark at the tip of the GR peak
  if(!is.na(GR.mark)){
    if(is.na(GR.mark.col)){
      GR.mark.col <- adjustcolor(line.col, alpha.line)
    }else{
      GR.mark.col <- adjustcolor(GR.mark.col, alpha.GR.mark)
    }
    points(x=prof.vec[1], y=0, col=GR.mark.col, pch=GR.mark,
           cex=GR.mark.cex, lwd=GR.mark.lwd)
  }
}


