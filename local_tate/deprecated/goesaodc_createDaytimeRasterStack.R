#' @export
#'
#' @title Create a daytime RasterStack for a specified date
#' 
#' @description Create a \code{RasterStack} from GOES AOD data files for the
#' date specified by \code{datetime}. Each \code{RasterLayer} contains
#' data from one Advanced Baseline Imager (ABI) scan during the specified time
#' period.
#'
#' If data for the specified time period is not found in the directory specified
#' by \code{setSatelliteDataDir()}, it will be downloaded in order to create the
#' \code{RasterStack}.
#'
#' The Z axis of the \code{RasterStack} is a character vector where each element
#' is the time stamp of the scan and has the format YYYYMMDDHHMMSS. This can be
#' accessed using the \code{raster::getZ()} function. Names of the
#' \code{RasterStack} are also time stamps of the scan, of the format XHH.MM.SS.
#'
#' The \code{bbox} parameter can be a vector of floats in c(lonLo, lonHi, latLo,
#' latHi) order or the return value from \code{sp::bbox()} or 
#' \code{raster::extent()}.
#'
#' The \code{dqfLevel} parameter can take a value of:
#'
#' \itemize{
#' \item{0}{ -- High quality retrieval flag}
#' \item{1}{ -- Medium quality retrieval flag}
#' \item{2}{ -- Low quality retrieval flag}
#' \item{3}{ -- No retrieval quality flag}
#' }
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param var var GOES data variable ("AOD, "DQF" or "ID"); Defaults to "AOD".
#' @param res Resolution of raster in degrees; Defaults to 0.1.
#' @param bbox Bounding box for the region of interest.
#' @param dqfLevel Data quality flag level.
#' @param timezone Timezone used to interpret \code{datetime}.
#' @param verbose Logical flag to print stacking progress; Defaults to FALSE.
#'
#' @return RasterStack
#'
#' @examples
#' \dontrun{
#' library(MazamaSpatialUtils)
#' library(MazamaSatelliteUtils)
#' 
#' setSpatialDataDir("~/Data/Spatial")
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' # Kincade fire region
#' kincade_bbox <- c(-124, -120, 36, 39)
#'
#' dayStack <- goesaodc_createDaytimeRasterStack(
#'   satID = "G16",
#'   datetime = "2019-10-27",
#'   timezone = "America/Los_Angeles",
#'   bbox = kincade_bbox,
#'   dqfLevel = 2,
#'   verbose = TRUE
#' )
#'
#' # Town of Tomales
#' latitude  <- 38.245
#' longitude <- -122.906
#' 
#' tb <- raster_createLocationTimeseries(
#'   rasterStack = dayStack,
#'   longitude = longitude,
#'   latitude = latitude,
#'   bbox = kincade_bbox
#' )
#'
#' plot(
#'   x = tb$datetime,
#'   y = tb$aod,
#'   pch = 15,
#'   cex = 0.8,
#'   col = rgb(red = 0, green = 0, blue = 0, alpha = 0.8),
#'   main = "Tomales, CA - 2019-10-27",
#'   xlab = "Time (UTC)",
#'   ylab = "AOD"
#' )
#' }

goesaodc_createDaytimeRasterStack <- function(
  satID = NULL,
  datetime = NULL,
  var = "AOD",
  res = 0.1,
  bbox = NULL,
  dqfLevel = NULL,
  timezone = NULL,
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(dqfLevel)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) ) {
    stop("Must specify GOES satellite ID (G16 or G17)")
  }
  
  # Use timezone from POSIXt datetime, or passed in timezone
  if ( lubridate::is.POSIXt(datetime) ) {
    timezone <- lubridate::tz(datetime)
  } else {
    if ( !(timezone %in% OlsonNames()) ) {
      stop(sprintf("timezone \"%s\" is not recognized", timezone))
    }
  }
  
  # ----- Create RasterStack ---------------------------------------------------
  
  daylight <- getDaylightHours(datetime = datetime, timezone = timezone)
  
  sunrise <- daylight$sunrise
  sunset <- daylight$sunset
  
  dayStack <- goesaodc_createRasterStack(
    satID = satID, 
    datetime = sunrise,
    endtime = sunset,
    dqfLevel = dqfLevel,
    verbose = verbose
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(dayStack)
  
}
