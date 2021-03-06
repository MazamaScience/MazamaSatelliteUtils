% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDaylightHours.R
\name{getDaylightHours}
\alias{getDaylightHours}
\title{Calculate the sunrise and sunset hours for a time and place}
\usage{
getDaylightHours(
  datetime = NULL,
  longitude = NULL,
  latitude = NULL,
  bbox = NULL,
  timezone = NULL,
  isJulian = FALSE
)
}
\arguments{
\item{datetime}{Datetime as a Ymd HMS or Julian formatted string, or a 
\code{POSIXct}.}

\item{longitude}{Longitude of the location in decimal degrees E.}

\item{latitude}{Latitude of the location in decimal degrees N.}

\item{bbox}{Bounding box for the region.}

\item{timezone}{Timezone used to interpret \code{datetime}.}

\item{isJulian}{Logical flag determining whether \code{datetime} should be 
interpreted as a Julian date with day of year as a decimal number. Defaults 
to FALSE.}
}
\value{
List containing \code{POSIXct} \code{sunrise} and \code{sunset} times 
for the specified date.
}
\description{
Returns the sunrise and sunset hours for specified dates within 
a bounding box based on sun ephemerides calculations. Sunrise is calculated
using the midpoint of the east edge of the bounding box while sunset is
calculated using the midpoint of the west edge.

The bounding box may be specified via any of the following methods:

\itemize{
\item{}{Olson timezone, either as part of a POSIXt datetime, or specified 
via the \code{timezone} parameter}
\item{}{Geographic Bounding Box, either as a vector of floats in 
\code{c(lonLo, lonHi, latLo,latHi)} order, or the return value from 
\code{sp::bbox()}, or \code{raster::extent()} }
\item{}{Longitude and latitude specified in decimal degrees}
}

If more than one of the above are specified, the order of preference for 
determining the bounding box is:

\enumerate{
\item{\code{longitude, latitude} passed in -- location +/- 0.1 degrees}
\item{\code{bbox} passed in -- use unchanged}
\item{\code{timezone} passed in -- use bbox of the timezone}
\item{\code{datetime} is \code{POSIXct} -- use bbox of the timezone attribute}
}
}
\examples{
\donttest{
library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)

# Get sunrise and sunset by lat/lon and datetime
getDaylightHours(
  datetime = "2019-09-06", 
  longitude = -123.245, 
  latitude = 42.861
)

# Get sunrise and sunset by bbox and datetime
getDaylightHours(
  datetime = "2019-09-06",
  bbox = c(-124.566, -116.463, 41.991, 46.292) # Oregon
) 

# Get sunrise and sunset by datetime and timezone
getDaylightHours(
  datetime = "2019-09-06",
  timezone = "America/Los_Angeles"
)

# Get sunrise and sunset for a vector of datetimes
getDaylightHours(
  datetime = c("2019-06-21", "2019-09-23", "2019-12-22"),
  longitude = -123.245, 
  latitude = 42.861
)
}
}
