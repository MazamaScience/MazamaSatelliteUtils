#' @export
#'
#' @title Create a daytime RasterStack for a specified date
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Desired datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param longitude Target longitude used to calcualte daylight hours.
#' @param latitude Target latitude used to calculate daylight hours.
#' @param var variable ("AOD, "DQF" or "ID")
#' @param res resolution of raster in degrees
#' @param bbox Bounding box for the region of interest.
#' @param dqfLevel Data quality flag level.
#' @param timezone timezone in which to interpret the \code{datetime}.
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
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#'
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusStates")
#'
#' # Define the region of interest (Milepost 97 Fire in Oregon)
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' bbox_oregon <- sp::bbox(oregon)
#' longitude <- -123.245
#' latitude <-   42.861
#'
#' dateLocal <- lubridate::ymd("2019-08-01", tz = "America/Los_Angeles")
#'
#' dayStack <- goesaodc_createDaytimeRasterStack(
#'   satID = "G16",
#'   dateLocal,
#'   longitude = -123.32,
#'   latitude = 42.88,
#'   bbox = bbox_oregon
#' )
#' 
#' tb <- raster_createLocationTimeseries(dayStack,
#'                                       longitude = longitude,
#'                                       latitude = latitude,
#'                                       bbox = bbox_oregon)
#'
#' plot(x = tb$datetime, y = tb$aod,
#'      pch = 15, cex = 0.8, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.8),
#'      main = dateLocal, xlab = "Time (PDT)", ylab = "AOD")
#' }

goesaodc_createDaytimeRasterStack <- function(
  satID = NULL,
  datetime = NULL,
  longitude = NULL,
  latitude = NULL,
  var = "AOD",
  res = 0.1,
  bbox = NULL,
  dqfLevel = NULL,
  timezone = "UTC"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  
  datetime <- MazamaCoreUtils::parseDatetime(datetime, timezone)
  
  # ----- Calculate daylight hours ---------------------------------------------
  
  timezone <- MazamaSpatialUtils::getTimezone(longitude, latitude,
                                              countryCodes = c("US"))
  
  # Gather local timeinfo for the requested datetime
  timeInfo <- PWFSLSmoke::timeInfo(datetime,
                                   longitude = longitude,
                                   latitude = latitude,
                                   timezone = timezone)
  
  # Now that we have the local sunrise and sunset times for the date we convert
  # them back to UTC times
  sunriseUTC <- lubridate::with_tz(timeInfo$sunrise, tzone = "UTC")
  sunsetUTC <- lubridate::with_tz(timeInfo$sunset, tzone = "UTC")
  
  # Round and contract the boundary hours
  sunriseHourUTC <- lubridate::ceiling_date(sunriseUTC, unit = "hour")
  sunsetHourUTC <- lubridate::floor_date(sunsetUTC, unit = "hour")
  
  # Get all the UTC hours between the local sunrise and sunset hours
  hours <- seq.POSIXt(from = sunriseHourUTC, to = sunsetHourUTC, by = "hour")
  hours <- strftime(hours, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # ----- Create a single-day rasterStack --------------------------------------
  
  # Create a rasterStack for each hour and add them all to one "day" rasterStack
  dayStack <- raster::stack()
  
  for (hour in hours) {
    
    result <- try({
      
      hourStack <- goesaodc_createHourlyRasterStack(
        satID = satID,
        datetime = hour,
        var = var,
        res = 0.1,
        bbox = bbox,
        dqfLevel = dqfLevel
      )
      
      # Combine the rasters and timestamps of the day and hour stacks
      zDay <- raster::getZ(dayStack)
      zHour <- raster::getZ(hourStack)
      
      dayStack <- raster::stack(dayStack, hourStack)
      dayStack <- raster::setZ(dayStack, c(zDay, zHour))
      
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      stop(result)
    } else {
      print(paste0("Stacked hour: ", hour))
    }
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(dayStack)
  
}
