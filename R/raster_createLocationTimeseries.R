#' @export
#' 
#' @title Create a Tibble of AOD readings for a location over a given hour
#' 
#' @description Creates a time series Tibble of AOD values for 1 hour at the
#' given location. The readings are approximated by sampling nearby AOD raster 
#' cells.
#' 
#' @param rasterStack A GOES AOD \code{RasterStack}.
#' @param longitude Longitude of the location of interest in decimal degrees E.
#' @param latitude Latitude of the location of interest in decimal degrees N.
#' @param bbox Bounding box for the region of interest.
#' @param method Determines how a value is extracted from the location if a
#' buffer is not used. Can be set to either "simple", which returns the exact 
#' value of the cell the coordinates fall in, or "bilinear", which interpolates 
#' the four nearest raster cell values.
#' @param buffer Radius for a circluar region around the location. If multiple
#' cells fall within this region then all of their values are aggregated by the
#' 'fun' method. Radius is measured in map units (typically meters).
#' @param fun Function for aggregating all the values in the buffer (if buffer 
#' is used)
#' 
#' @return Tibble
#' 
#' @examples
#' \dontrun{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' loadSpatialData("USCensusStates")
#' 
#' # Oregon on August 1, 2019 at 12pm (Milepost 97 Fire)
#' datetime <- "2019-08-01 19"
#' timezone <- "UTC"
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' bbox_oregon <- sp::bbox(oregon)
#' lon <- -123.245
#' lat <- 42.861
#' 
#' # Gather all the raster layers for the given hour into a stack
#' rasterStack <- goesaodc_createRasterStack(
#'   satID = "G16",
#'   datetime = datetime,
#'   timezone = timezone, 
#'   bbox = bbox_oregon,
#'   res = 0.05,
#'   dqfLevel = 2,
#'   verbose = TRUE
#' )
#' 
#' rasterAvg <- raster::mean(rasterStack, na.rm = TRUE)
#' tb <- raster_createLocationTimeseries(
#'   rasterStack = rasterStack,
#'   longitude = lon,
#'   latitude = lat,
#'   bbox = bbox_oregon,
#'   buffer = 1000
#' )
#' 
#' # Plot AOD data
#' pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
#' par(mfrow = c(1, 2))
#' 
#' # Raster plot on left
#' raster::plot(
#'   rasterAvg,
#'   main = "Average AOD",
#'   col = pal_aod(50),
#'   xlim = c(-125, -122),
#'   ylim = c(42, 44)
#' )
#' plot(oregon, add = TRUE)
#' 
#' # Time series plot on right
#' points(x = c(lon), y = c(lat), cex = 2.0, pch = 3, lwd = 1.5)
#' plot(
#'   x = tb$datetime,
#'   y = tb$aod,
#'   pch = 15,
#'   cex = 1,
#'   main = datetime,
#'   xlab = "Time",
#'   ylab = "AOD"
#' )
#' }

raster_createLocationTimeseries <- function(
  rasterStack = NULL,
  longitude = NULL,
  latitude = NULL,
  bbox = NULL,
  method = "simple",
  buffer = NULL,
  fun = mean
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(rasterStack)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(bbox)
  MazamaCoreUtils::stopIfNull(buffer)
  
  # ----- Create AOD tibble ----------------------------------------------------
  
  # Format location coordinates
  location <- data.frame(lon = longitude, lat = latitude)
  
  # Format the timestamps
  timeStrings <- attributes(rasterStack)$z$time
  datetime <- MazamaCoreUtils::parseDatetime(
    timeStrings, 
    timezone = "UTC"
  )
  
  # Store the aggregated AOD value of the location in each layer of the stack
  aod <- c()
  for ( i in c(1:length(rasterStack[1])) ) {
    layer <- rasterStack[[i]]
    aod[i] <- raster::extract(
      x = layer,
      y = location, 
      method = method,
      buffer = buffer,
      fun = fun
    )
  }
  
  # Create a tibble with the AOD values and their timestamps
  tb <- tibble::tibble(datetime, aod)
  
  # ----- Return ---------------------------------------------------------------
  
  return(tb)
  
}
