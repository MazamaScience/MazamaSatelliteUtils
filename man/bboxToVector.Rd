% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{bboxToVector}
\alias{bboxToVector}
\title{Convert and validate bbox objects or strings into standard format}
\usage{
bboxToVector(bbox = NULL)
}
\arguments{
\item{bbox}{Bounding box for the region of interest.}
}
\value{
a vector of floats in c(lonLo, lonHi, latLo, latHi) order.
}
\description{
Accepts a variety of bbox formats, validates that they are
numerically sane and returns a vector of floats in c(lonLo, lonLo, latLo, 
latHi) order. Input can be a vector of floats in c(lonLo, lonHi, latLo, 
latHi) order or the return value from \code{sp::bbox()} or 
\code{raster::extent()}.
}
\examples{
\donttest{
library(MazamaSpatialUtils)
library(MazamaSatelliteUtils)

setSpatialDataDir("~/Data/Spatial")
setSatelliteDataDir("~/Data/Satellite")

loadSpatialData("USCensusStates")

oregon <- subset(USCensusStates, stateCode == "OR")
bbox <- sp::bbox(oregon)

scanFile <- goesaodc_downloadScanFiles(
  satID = "G16", 
  datetime = "2019-09-06 18:00", 
  timezone = "UTC"
)

scanRaster <- goesaodc_createScanRaster(
  filename = scanFile,
  cellSize = 0.1,
  dqfLevel = 2
)

extent <- raster::extent(scanRaster)

# Convert bbox
bbox
bboxToVector(bbox)

# Convert extent
extent
bboxToVector(extent)

# Convert low/hi mixup
bboxToVector(c(-116, -124, 42, 46))
}

}
