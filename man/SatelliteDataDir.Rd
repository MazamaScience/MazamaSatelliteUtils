% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MazamaSatelliteUtils.R
\docType{data}
\name{SatelliteDataDir}
\alias{SatelliteDataDir}
\title{Directory for Satellite Data}
\format{
Absolute path string.
}
\description{
This package maintains an internal directory location which 
users can set using \code{setSatelliteDataDir()}. All package functions use 
this directory whenever datasets are created or loaded.

The default setting when the package is loaded is \code{getwd()}.
}
\seealso{
\link{getSatelliteDataDir}

\link{setSatelliteDataDir}
}
\keyword{environment}
