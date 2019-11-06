#' @export
#' 
#' @title Calculates the sunrise and sunset hours for a specific date and 
#' location
#' 
#' @description Returns the sunrise and sunset hours for specified dates within 
#' a bounding box based on sun ephemerides calculations. Sunrise is calcualted
#' using the midpoint of the east edge of the bounding box while sunset is
#' calculated using the midpoint of the west edge.
#' 
#' The bounding box may be specified via any of the following methods:
#'
#' \itemize{
#' \item{}{Olson timezone, either as part of a POSIXt datetime, or specified 
#' via the \code{timezone} parameter}
#' \item{}{Geographic Bounding Box, either as a vector of floats in 
#' \code{c(lonLo, lonHi, latLo,latHi)} order, or the return value from 
#' \code{sp::bbox()}, or \code{raster::extent()} }
#' \item{}{Longitude and Latitude specified in decimal degrees}
#' }
#' 
#' If more than one of the above are specified, the order of preference for 
#' determining the bounding box is:
#' 
#' \enumerate{
#' \item{\code{longitude, latitude} passed in -- location +/- 0.1 degrees}
#' \item{\code{bbox} passed in -- use unchanged}
#' \item{\code{timezone} passed in -- use bbox of the timezone}
#' \item{\code{datetime} is \code{POSIXct} -- use bbox of the timezone attribute}
#' }
#' 
#' @param datetime Specific date for which daylight hours are requested.
#' @param longitude Longitude of the location of interest in decimal degrees E.
#' @param latitude Latitude of the location of interest in decimal degrees N.
#' @param bbox Bounding box for the region of interest, Default: CONUS.
#' @param timezone Timezone in which to interpret the \code{datetime}.
#' @param isJulian Logical value determining whether datetime should be 
#' interpreted as a Julian date with day of year as a decimal number.
#' @return List containing \code{POSIXct} \code{sunrise} and \code{sunset} times 
#' for the specified date.
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' # EX 1. Get Sunrise and Sunset by Lat/Lon and Datetime
#' daylight <- getDaylightHours(
#'   datetime = "2019-09-06", 
#'   longitude = -123.245, 
#'   latitude = 42.861
#' )
#' 
#' # EX 2. Get Sunrise and Sunset by BBOX and datetime
#' daylight <- getDaylightHours(
#'   datetime = "2019-09-06",
#'   bbox = c(-124.566, -116.463, 41.991, 46.292)
#' ) # Oregon
#' 
#' # EX 3. Get Sunrise and Sunset by Datetime and Timezone
#' daylight <- getDaylightHours(
#'   datetime = "2019-09-06",
#'   timezone = "America/Los_Angeles")
#' }
#' 
#' @rdname getDaylightHours
#' 

getDaylightHours <- function(
  datetime = NULL,
  longitude = NULL,
  latitude = NULL,
  bbox = NULL,
  timezone = NULL,
  isJulian = FALSE
) {
  
  # ---- Validate Parameters ---------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(datetime)
  
  # ----- Determine bbox -------------------------------------------------------
  
  if ( !is.null(longitude) && !is.null(latitude) ) {
    
    # Pad to roughly 10km. This is ugly, but serves the purpose.
    bbox <- bboxToVector( c(longitude, longitude + .1, latitude, latitude + .1))
    
  } else if ( !is.null(bbox) ) {
    
    # Do nothing
    
  } else if ( !is.null(timezone) ) {
    
    if ( !timezone %in% OlsonNames() )
      stop(sprintf("timezone \"%s\" is not recognized", timezone))
    
    # Get bbox of timezone
    param_tz <- timezone
    tz <- subset(MazamaSpatialUtils::SimpleTimezones, timezone == param_tz)
    tz_bbox <- sp::bbox(tz)
    bbox <- bboxToVector(tz_bbox)
    
  } else if ( lubridate::is.POSIXt(datetime) ) {
    
    # Get timezone attribute, then bbox of timezone
    param_tz <- attr(datetime, "tzone")
    tz <- subset(MazamaSpatialUtils::SimpleTimezones, timezone == param_tz)
    tz_bbox <- sp::bbox(tz)
    bbox <- bboxToVector(tz_bbox)
    
  } else {
    
    stop("Cannot determine bounding box from arguments")
    
  }
  
  # ----- Prepare ephemeris arguments ------------------------------------------
  
  # Get the bbox components
  boundaries <- bboxToVector(bbox)
  w <- boundaries[1]
  e <- boundaries[2]
  s <- boundaries[3]
  n <- boundaries[4]
  
  mid_lon <- w + (e - w) / 2
  mid_lat <- s + (n - s) / 2
  
  # Get the timezone in the bbox center
  timezone <- MazamaSpatialUtils::getTimezone(lon = mid_lon, 
                                              lat = mid_lat, 
                                              countryCodes = c("US"))
  
  # Ensure we have a POSIXt datetime
  datetime <- MazamaCoreUtils::parseDatetime(datetime = datetime, 
                                             timezone = timezone, 
                                             isJulian = isJulian)
  
  # ----- Calculate ephemeris from bbox ----------------------------------------
  
  west_edge <- matrix(c(w, mid_lat), nrow = 1)
  east_edge <- matrix(c(e, mid_lat), nrow = 1)
  
  # sunrise for mid_lat on East edge
  sunriseDF <- maptools::sunriset(east_edge, 
                                  datetime, 
                                  direction = "sunrise",
                                  POSIXct.out = TRUE)
  
  # sunset for mid_lat West edge
  sunsetDF <- maptools::sunriset(west_edge, 
                                 datetime, 
                                 direction = "sunset",
                                 POSIXct.out = TRUE)
  
  # ----- Return ---------------------------------------------------------------
  
  daylight <- list(
    "sunrise" = sunriseDF[, 2], 
    "sunset" = sunsetDF[, 2]
  )
  
  return(daylight)
  
}
