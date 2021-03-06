% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-time.R
\name{isDaylight}
\alias{isDaylight}
\title{Determine whether times are during daylight within a region}
\usage{
isDaylight(datetime = NULL, timezone = "UTC", bbox = bbox_CONUS)
}
\arguments{
\item{datetime}{Datetime in any Ymd H [MS] format or \code{POSIXct}.}

\item{timezone}{Timezone used to interpret \code{datetime}. Defaults to UTC.}

\item{bbox}{Bounding box for the region of interest. Defaults to CONUS.}
}
\value{
Logical vector.
}
\description{
Determine whether times are during daylight within a region
}
\examples{
\donttest{
library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)

setSatelliteDataDir("~/Data/Satellite")

# Example with bbox from country shape
mx <- subset(SimpleCountries, countryCode == "MX")

mx_bbox <- sp::bbox(mx)

isDaylight(
  datetime = "2019-09-06 12",
  bbox = mx_bbox,
  timezone = "UTC"
) 

# Example with bbox from raster::extent()

scanFile <- goesaodc_downloadScanFiles(
  satID = "G16", 
  datetime = "2019-09-06 18:00", 
  timezone = "UTC"
)

raster <- goesaodc_createScanRaster(
  filename = scanFile,
  cellSize = 0.1,
  dqfLevel = 2
)

extent <- raster::extent(raster)

isDaylight(
  datetime = "2019-09-06 12:00",
  bbox = extent,
  timezone = "UTC"
)
}
}
