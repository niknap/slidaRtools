% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc.field.vert.index.BA.layer.TUM_function.R
\name{calc.field.vert.index.BA.layer.TUM}
\alias{calc.field.vert.index.BA.layer.TUM}
\title{Calculate vertical structure index}
\usage{
calc.field.vert.index.BA.layer.TUM(H.vec, D.vec, CL.vec)
}
\arguments{
\item{H.vec}{Vector of tree heights [m]}

\item{D.vec}{Vector of tree DBH [m]}

\item{CL.vec}{Vector of tree crown length (tree height - crown base height) [m]}
}
\value{
Index of vertical forest structure
}
\description{
Function for vertical index from TUM calculated from inventory data.
Basal area of trees will be distributed based on tree's crown length
}
\examples{
calc.field.vert.index.BA.layer.TUM(40,1,20)
}
\keyword{forest}
\keyword{heterogeneity}
\keyword{structure}
\keyword{vertical}
