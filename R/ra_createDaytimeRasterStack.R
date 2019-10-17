#' ra_createDaytimeRasterStack.R
#' 
#' 
#' @export
#'
#' @title Create a daytime RasterStack for a specified date
#'
#' @examples
#' \dontrun{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' MazamaCoreUtils::initializeLogging(logDir = "D:Mazama/Data/Logs")
#'
#' # Define the region of interest (Milepost 97 Fire in Oregon)
#' bbox_oregon <- c(-124.56624, -116.46350, 41.99179, 46.29203)
#' longitude <- -123.245
#' latitude <-   42.861
#'
#' datetime <- MazamaCoreUtils::parseDatetime(datetime = "2019-09-06", 
#' timezone = "America/Los_Angeles")
#'
#' dayStack <- ra_createDaytimeRasterStack(
#'   satID = "G16",
#'   datetime = datetime,
#'   longitude = longitude,
#'   latitude = latitude,
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
#'      main = datetime, xlab = "Time (PDT)", ylab = "AOD")
#' }

ra_createDaytimeRasterStack <- function(
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
  
  # VALIDATE IS TIME BEING PASSED IN IS ALREADY A POSIX TIME WITH timezone
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(datetime)[1] %in% time_classes ) {
    timezone <- attr(datetime,"tzone")
  }
  
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
      
      hourStack <- ra_createHourlyRasterStack(
        satID = satID,
        startTime = hour,
        var = var,
        res = 0.1,
        bbox = bbox,
        dqfLevel = dqfLevel)
      
      # Combine the rasters and timestamps of the day and hour stacks
      zDay <- raster::getZ(dayStack)
      zHour <- raster::getZ(hourStack)
      
      dayStack <- raster::stack(dayStack, hourStack)
      dayStack <- raster::setZ(dayStack, c(zDay, zHour))
      
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      if ( stringr::str_detect(err_msg, "No data for selected region") ) {
        # Warn but don't stop
        #if ( MazamaCoreUtils::logger.isInitialized() ) {
          #MazamaCoreUtils::logger.warn("No data found for hour %s", hour)
        #}
        print(paste0("No data found for hour %s", hour))
      } else {
        stop(result)
      }
    } else {
      print(paste0("Stacked hour: ", hour, " UTC"))
    }
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(dayStack)
  
}
