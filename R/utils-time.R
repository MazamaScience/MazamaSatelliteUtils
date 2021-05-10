#' @export
#' 
#' @title Determine whether times are during daylight within a region
#' 
#' @param datetime Datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime}; Defaults to UTC.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' 
#' @return Logical vector.
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' # Example with bbox from country shape
#' mx <- subset(SimpleCountries, countryCode == "MX")
#' 
#' mx_bbox <- sp::bbox(mx)
#' 
#' isDaylight(
#'   datetime = "2019-09-06 12",
#'   bbox = mx_bbox,
#'   timezone = "UTC"
#' ) 
#' 
#' # Example with bbox from raster::extent()
#' 
#' scanFile <- goesaodc_downloadScanFiles(
#'   satID = "G16", 
#'   datetime = "2019-09-06 18:00", 
#'   timezone = "UTC"
#' )
#' 
#' raster <- goesaodc_createScanRaster(
#'   filename = scanFile,
#'   cellSize = 0.1,
#'   dqfLevel = 2
#' )
#' 
#' extent <- raster::extent(raster)
#' 
#' isDaylight(
#'   datetime = "2019-09-06 12:00",
#'   bbox = extent,
#'   timezone = "UTC"
#' )
#' }

isDaylight <- function(
  datetime = NULL,
  timezone = "UTC",
  bbox = bbox_CONUS
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(bbox)
  
  datetime <- MazamaCoreUtils::parseDatetime(datetime, timezone = timezone)
  
  # ----- Extract boundaries ---------------------------------------------------
  
  boundaries <- bboxToVector(bbox)
  w <- boundaries[1]
  e <- boundaries[2]
  s <- boundaries[3]
  n <- boundaries[4]
  
  mid_lat <- s + (n - s) / 2
  
  # ----- Calculate sunrise and sunset -----------------------------------------
  
  # maptools::sunriset() requires a matrix
  west_edge <- matrix(c(w, mid_lat), nrow = 1)
  east_edge <- matrix(c(e, mid_lat), nrow = 1)
  
  # Sunrise for mid_lat on East edge
  sunriseDF <- maptools::sunriset(
    east_edge, 
    datetime, 
    direction = "sunrise",
    POSIXct.out = TRUE
  )
  
  # Sunset for mid_lat West edge
  sunsetDF <- maptools::sunriset(
    west_edge, 
    datetime, 
    direction = "sunset",
    POSIXct.out = TRUE
  )
  
  # Extract vectors
  sunrise <- sunriseDF[, 2] 
  sunset <- sunsetDF[, 2]
  
  daylightMask <- datetime > sunrise & datetime < sunset
  
  # ----- Return ---------------------------------------------------------------
  
  return(daylightMask)
  
}
