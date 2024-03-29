% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_spatial_index_function.R
\name{calc_spatial_index}
\alias{calc_spatial_index}
\title{Calculate spatial indices for points in 2D space}
\usage{
calc_spatial_index(
  xcor,
  ycor,
  res = 1,
  minx = NA,
  miny = NA,
  maxx = NA,
  maxy = NA,
  exclude.maxx = T
)
}
\arguments{
\item{xcor}{X-coordinate of the point}

\item{ycor}{Y-coordinate of the point}

\item{res}{side length of one spatial subunit}

\item{minx}{minimum X-coordinate}

\item{miny}{minimum Y-coordinate}

\item{maxx}{maximum X-coordinate}

\item{maxy}{maximum Y-coordinate}

\item{exclude.maxx}{boolean to set whether the given maxx should be excluded or included in the indexing}
}
\value{
spatial index for the input point
}
\description{
Function that returns spatial indices (plot numbers) for given coordinate pairs.
E.g. for hectares res = 100 m
}
\examples{
in progress
}
\author{
Nikolai Knapp
}
\keyword{index}
\keyword{number}
\keyword{plot}
\keyword{spatial}
