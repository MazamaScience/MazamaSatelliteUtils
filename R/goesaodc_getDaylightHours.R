#' @export
#' 
#' @title Calculates the sunrise and sunset hours for a specific date and 
#' location
#' 
#' @description Returns the sunrise and sunset hours for a specific date and 
#' location based on sun ephemerides calculations.  Location can be specified 
#' via any of the following methods.
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
#' @param datetime specific date for which daylight hours are requested
#' @param latitude geographic latitude, in decimal degrees, for point of 
#' interest
#' @param longitude geographic longitude, in decimal degrees, for point of 
#' interest
#' @param timezone timezone in which to interpret the \code{datetime}
#' @param bbox Bounding box for the region of interest
#' @param julian logical, optional format to specify \code{datetime} parameter
#' @return day_info List which contains sunrise and sunset times as POSIXt 
#' values
#' 
#' @examples 
#' \dontrun{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' EX 1. Get Sunrise and Sunset by Lat/Lon and Datetime
#' dayInfo <- goesaodc_getDaylightHours(
#' datetime = "2019-09-06", 
#' longitude = -123.245, 
#' latitude = 42.861)
#' 
#' EX 2. Get Sunrise and Sunset by BBOX and datetime
#' dayInfo <- goesaodc_getDaylightHours(
#' datetime = "2019-09-06",
#' bbox = c(-124.566, -116.463, 41.991, 46.292)) # Oregon
#' 
#' EX 3. Get Sunrise and Sunset by Datetime and Timezone
#' dayInfo <- goesaodc_getDaylightHours(
#' datetime = "2019-09-06",
#' timezone = "America/Los_Angeles")
#' }
#' 
#' @rdname goesaodc_getDaylightHours
#' 

goesaodc_getDaylightHours <- function(
  datetime = NULL,
  latitude = NULL,
  longitude = NULL,
  timezone = NULL,
  bbox = NULL,
  julian = FALSE
) {
  
  MazamaCoreUtils::stopIfNull(datetime)
  
  # Check whether both bbox and lat/lon exist. Only need one at start ----------
  if ( !is.null(bbox) && !is.null(latitude) ) {
    stop("Cannot process both BBOX and Lat/Lon at the same time. Pick one.")
  }
  
  # ---- Check if a timezone has been passed in with a POSIXt ------------------
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(datetime)[1] %in% time_classes ) {
    timezone <- attr(datetime, "tzone")
  }
  
  # If we only have a datetime and timezone, we need the timezone's bbox -------
  if (is.null(bbox) && is.null(latitude) && !is.null(timezone)) {
    param_tz <- timezone
    tz <- subset(MazamaSpatialUtils::SimpleTimezones, timezone == param_tz,)
    tz_bbox <- sp::bbox(tz)
    bbox <- bboxToVector(tz_bbox)
  }
  
  # ---- If lat/lon given, Pad them to create a bbox ---------------------------
  if ( !is.null(latitude) && !is.null(longitude)) {
    # Pad to roughly 10km. This is ugly, but serves the purpose.
    bbox <- bboxToVector( c(longitude, longitude + .1, latitude, latitude + .1))
  }
  
  # ALL CALCULATIONS RELY ON THESE VARIABLES FOR BBOX-DERIVED EPHEMERIS. EVEN IF
  # A BBOX IS NOT PASSED IN, WE MAKE ONE IN ORDER TO CALCULATE DAYLIGHT VALUES
  mid_lon <- NULL
  mid_lat <- NULL
  w <- NULL
  e <- NULL
  s <- NULL
  n <- NULL
  
  # ---- Unwind the bbox (THIS MAY NEED TO MOVE DOWN IN THE FUNCTION) ----------
  if ( !is.null(bbox) ) {
    boundaries <- bboxToVector(bbox)
    w <- boundaries[1]
    e <- boundaries[2]
    s <- boundaries[3]
    n <- boundaries[4]
    
    mid_lon <- w + (e - w) / 2
    mid_lat <- s + (n - s) / 2
  }
  
  # ---- Figure out Timezone info, from whatever we have -----------------------
  if ( is.null(timezone) && is.null(bbox) ) {
    # Infer timezone using lat/lon
    MazamaCoreUtils::stopIfNull(latitude, 
    "Can't calculate timezone. Need timezone, lat/lon, or  bbox.")
    timezone <- MazamaSpatialUtils::getTimezone(lon = longitude, 
                                                lat = latitude, 
                                                countryCodes = c("US"))
    # Infer timezone from bbox center
  } else if ( is.null(timezone) && !is.null(bbox) ) {
    timezone <- MazamaSpatialUtils::getTimezone(lon = mid_lon, 
                                                lat = mid_lat, 
                                                countryCodes = c("US"))
  }
  
  # ---- Ensure we have a POSIXt datetime --------------------------------------
  datetime <- MazamaCoreUtils::parseDatetime(datetime = datetime, 
                                             timezone = timezone, 
                                             julian = julian)
  
  
  
  # ---- CALCULATE EPHEMERIS BY BBOX -------------------------------------------
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
  
  # Extract vectors
  sunrise <- sunriseDF[, 2]
  sunset <- sunsetDF[, 2]
  
  day_info <- list("sunrise" = sunrise, "sunset" = sunset)
  
  return(day_info)
}