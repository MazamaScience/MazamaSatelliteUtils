#' @export
#' 
#' @title check if specified \code{datetime} is during the daylight hours of 
#' the specified \code{region}
#' 
#' @param datetime desired datetime in any Y-m-d H [MS] format or \code{POSIXct}
#' @param bbox bounding box for region of interest
#' @param timezone timezone in which to interpret the \code{datetime}
#' 
#' 
#' @return logical
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' # BBOX AS CREATED BY SP
#' 
#' library(MazamaSpatialUtils)
#' 
#' mx <- subset(SimpleCountries, countryCode == "MX")
#' 
#' mx_bbox <- bbox(mx)
#' 
#' isDaylight(
#'   datetime = "2019-09-06 12",
#'   bbox = mx_bbox,
#'   timezone = "UTC") 
#' 
#' # BBOX AS DEFINED BY CALLING ON raster::extent
#' G16_filepath <- system.file(
#'   "extdata", 
#'   "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc", 
#'   package = "MazamaSatelliteUtils")
#' 
#' nc <- ncdf4::nc_open(G16_filepath)
#' 
#' raster <- goesaodc_createRaster(nc, res = 0.1, dqfLevel = 2)
#' 
#' extent <- raster::extent(raster)
#' 
#' isDaylight(
#'   datetime = "2019-09-06 12",
#'   bbox = extent,
#'   timezone = "UTC")
#' }

isDaylight <- function(
  datetime = NULL,
  bbox = NULL,
  timezone = "UTC"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(bbox)
  
  datetime <- MazamaCoreUtils::parseDatetime(datetime, timezone)
  
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
  
  # ----- check if datetime is during daylight hours ---------------------------
  
  if ( datetime > sunrise && datetime < sunset ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}
