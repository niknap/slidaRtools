% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/in_cylinder_function.R
\name{in_cylinder}
\alias{in_cylinder}
\title{Check if point lies within a cylinder}
\usage{
in_cylinder(Xcor, Ycor, Zcor, Xctr, Yctr, Zbase, radius, height)
}
\arguments{
\item{Xcor}{X-coordinate of the point}

\item{Ycor}{Y-coordinate of the point}

\item{Zcor}{Z-coordinate of the point}

\item{Xctr}{X-coordinate of the center}

\item{Yctr}{Y-coordinate of the center}

\item{Zbase}{Z-coordinate of the lower end}

\item{radius}{...of the cylinder}

\item{height}{...of the cylinder}
}
\value{
boolean or matrix of booleans
}
\description{
Function that checks for each X-Y-coordinate pair, whether the point lies in a
cylinder of a given center, radius and height. Works with single numbers or matrices of
X- and Y-coordinates as input.
}
\examples{
in progress
}
\author{
Nikolai Knapp
}
\keyword{cylinder}
\keyword{in}
\keyword{shape}
