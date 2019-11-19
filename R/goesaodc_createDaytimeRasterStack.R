#' @export
#'
#' @title Create a daytime RasterStack for a specified date
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Desired datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param var variable ("AOD, "DQF" or "ID")
#' @param res resolution of raster in degrees
#' @param bbox Bounding box for the region of interest.
#' @param dqfLevel Data quality flag level.
#' @param timezone timezone in which to interpret the \code{datetime}.
#' @param verbose show progress of raster stacking. 
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
#' The \code{dqfLevel} parameter can take a value of:
#'
#' \itemize{
#' \item{0}{ -- High quality retrieval flag}
#' \item{1}{ -- Medium quality retrieval flag}
#' \item{2}{ -- Low quality retrieval flag}
#' \item{3}{ -- No retrieval quality flag}
#' }
#' 
#' The \code{bbox} parameter can be a vector of floats in c(lonLo, lonHi, latLo,
#' latHi) order or the return value from \code{sp::bbox()} or 
#' \code{raster::extent()}.
#'
#' @return RasterStack
#'
#' @examples
#' \dontrun{
#' 
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' loadSpatialData("USCensusStates")
#' 
#' calif <- subset(USCensusStates, stateCode == "CA")
#' 
#' bbox_ca   <- sp::bbox(calif)
#' satID     <- "G16"
#' datetime  <- "2019-10-23"
#' timezone  <- "America/Los_Angeles"
#' dqfLevel  <- 2
#' latitude  <- 38.448611 
#' longitude <- -122.704722
#'
#' dayStack <- goesaodc_createDaytimeRasterStack(
#'   satID = satID,
#'   datetime = datetime,
#'   timezone = timezone,
#'   bbox = bbox_ca,
#'   dqfLevel = dqfLevel
#' )
#' 
#' tb <- raster_createLocationTimeseries(dayStack,
#'                                       longitude = longitude,
#'                                       latitude = latitude,
#'                                       bbox = bbox_ca)
#'
#' plot(x = tb$datetime, y = tb$aod,
#'      pch = 15, cex = 0.8, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.8),
#'      main = "Santa Rosa, 2019-10-23", xlab = "Time (UTC)", ylab = "AOD")
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
  
  # VALIDATE IS TIME BEING PASSED IN IS ALREADY A POSIX TIME WITH timezone
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(datetime)[1] %in% time_classes ) {
    timezone <- attr(datetime,"tzone")
  }
  
  daylight <- getDaylightHours(datetime = datetime, 
                               timezone = timezone)
  
  sunrise <- daylight$sunrise
  sunset <- daylight$sunset
  
  dayStack <- goesaodc_createRasterStack(satID = satID, 
                                         datetime = sunrise,
                                         endTime = sunset,
                                         dqfLevel = dqfLevel,
                                         verbose = verbose)
  
  # ----- Return ---------------------------------------------------------------
  return(dayStack)
  
}
