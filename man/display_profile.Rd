% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display_profile_function.R
\name{display_profile}
\alias{display_profile}
\title{Plot vertical profile, e.g. of a Lidar dataset}
\usage{
display_profile(
  prof.vec,
  line.col = "black",
  fill.col = NA,
  add = F,
  maxx = NA,
  maxy = NA,
  alpha.line = 1,
  alpha.fill = 0.5,
  lwd = 2,
  lty = 1,
  GR.mark = NA,
  GR.mark.cex = 1,
  GR.mark.lwd = 2,
  GR.mark.col = NA,
  alpha.GR.mark = 1,
  xlab = bquote("Count of Lidar returns [ha"^-1 ~ "]"),
  ylab = "Height [m]",
  cex.axis = 1,
  cex.lab = 1,
  las = 1
)
}
\arguments{
\item{prof.vec}{Profile with values and height bins as names (derived from make.profile functions)}

\item{line.col}{Line color}

\item{fill.col}{Polygon fill color (set to NA if no fill is desired)}

\item{add}{If F a new graph is drawn, if T the profile is plotted into an existing graph}

\item{maxx}{X-axis maximum}

\item{maxy}{Y-axis maximum}

\item{alpha.line}{Transparency setting for the line}

\item{alpha.fill}{Transparency setting for the polygon fill}

\item{lwd}{Line width}

\item{lty}{Line type}

\item{GR.mark}{Character that should be displayed at the tip of the ground return peak (if none set to NA)}

\item{GR.mark.cex}{Size of the ground return marker}

\item{GR.mark.lwd}{Line width of the ground return marker}

\item{GR.mark.col}{Color of the ground return marker}

\item{alpha.GR.mark}{Transparency setting for the ground return marker}

\item{xlab}{X-axis titel}

\item{ylab}{Y-axis titel}

\item{cex.axis}{Font size of axis numbers}

\item{cex.lab}{Font size of axis titels}

\item{las}{Orientation of axis numbers}
}
\value{
Graphic of the profile
}
\description{
Function for profile plotting.
}
\examples{
in progress
}
\author{
Nikolai Knapp
}
\keyword{2D}
\keyword{cloud}
\keyword{display}
\keyword{graphics}
\keyword{lidar}
\keyword{plot}
\keyword{point}
\keyword{profile}
\keyword{visualization}
