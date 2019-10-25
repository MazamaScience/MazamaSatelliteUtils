#' @export 
#' @title goesaodc_getDaylightHours
#' @description Returns the sunrise and sunset hours for a location, timezone, 
#' or bounding box.
#' @param datetime Default: NULL
#' @param latitude Default: NULL
#' @param longitude Default: NULL
#' @param timezone Default: NULL
#' @param julian Default: FALSE
#' @return day_info List which contains sunrise and sunset times as POSIXt 
#' values
#' @examples 
#' \dontrun{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' EX 1. Get Sunrise and Sunset by Lat/Lon and Datetime - TO DO
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
#' EX 3. Get Sunrise and Sunset by Datetime and Timezone - TO DO
#' dayInfo <- goesaodc_getDaylightHours(
#' datetime = "2019-09-06",
#' timezone = "America/Los_Angeles")
#' }
#' @rdname goesaodc_getDaylightHours


goesaodc_getDaylightHours <- function(
  datetime = NULL,
  latitude = NULL,
  longitude = NULL,
  timezone = NULL,
  bbox = NULL,
  julian = FALSE
) {
  
  MazamaCoreUtils::stopIfNull(datetime)
  
  # ALL CALCULATIONS RELY ON THESE VARIABLES FOR BBOX-DERIVED EPHEMERIS
  mid_lon <- NULL
  mid_lat <- NULL
  w <- NULL
  e <- NULL
  s <- NULL
  n <- NULL
  
  if ( !is.null(bbox) ) {
    boundaries <- bboxToVector(bbox)
    print(boundaries)
    w <- boundaries[1]
    e <- boundaries[2]
    s <- boundaries[3]
    n <- boundaries[4]
    
    mid_lon <- w + (e - w) / 2
    mid_lat <- s + (n - s) / 2
  }
  print(mid_lon)
  print(mid_lat)
  # ---- Figure out Timezone info, from whatever we have -----------------------
  if ( is.null(timezone) && is.null(bbox) ) {
    MazamaCoreUtils::stopIfNull(latitude, "Can't calculate timezone. Either pass
                                 in a valid timezone, or provide a latitude and 
                                longitude or a bbox.")
    timezone <- MazamaSpatialUtils::getTimezone(lon = longitude, 
                                                lat = latitude, 
                                                countryCodes = c("US"))
    
  } else if ( is.null(timezone) && !is.null(bbox) ) {
    timezone <- MazamaSpatialUtils::getTimezone(lon = mid_lon, 
                                                lat = mid_lat, 
                                                countryCodes = c("US"))
  }
  
  # ---- Ensure we have a POSIXt datetime 
  datetime <- MazamaCoreUtils::parseDatetime(datetime, timezone, julian = julian)
  
  # USE CASE 2 - BY BBOX (EVERYTHING USES THIS)
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