#' @export
#' 
#' @title Create a Tibble for a location's AOD readings at a given hour
#' 
#' @param rasterStack a RasterStack that holds layers for every ...
#' @param lon longitude of the location.
#' @param lat latitude of the location.
#' @param bbox bounding box for the rasters.
#' @param method determines how a value is extracted from the location cell if a
#' buffer is not defined. Can be set to either "simple", which returns the 
#' direct location cell value, or "bilinear", which interpolates 4 nearest 
#' raster cell values.
#' @param buffer radius for the circluar region around the location. If multiple
#' cells fall within this region then all of their values are aggregated by the
#' 'fun' method. Radius is measured in map units (typically meters).
#' @param fun function for aggregating all the values in the buffer (if buffer 
#' is used)
#' 
#' @description Creates a time series Tibble for AOD at the given coordinates. 
#' The readings are approximated by tracking the AOD values for raster cells
#' close to the coordinates.
#' 
#' @return Tibble
#' 
#' @examples
#' \donttest{
#' # Library setup
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusStates")
#' 
#' # Oregon on July 31, 2019 at 9am (Milepost 97 Fire)
#' startdate <- lubridate::ymd_h("2019-07-31 16", tz = "UTC")
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' bbox_oregon <- sp::bbox(oregon)
#' 
#' # Gather all the raster layers for the given hour into a stack
#' rasterStack <- goesaodc_createHourlyRasterStack(startdate = startdate, 
#'                                                 bbox = bbox_oregon,
#'                                                 res = 0.1)
#' 
#' tb <- raster_createLocationTimeseries(rasterStack = rasterStack,
#'                                       longitude = -123.26, latitude = 42.86, 
#'                                       bbox = bbox_oregon,
#'                                       buffer = 1000)
#' 
#' # Plot AOD time series
#' par(mfrow = c(1, 2))
#' raster::plot(rasterStack[[1]])
#' plot(oregon, add = TRUE)
#' points(x = c(-123.26), y = c(42.86), col = "red", cex = 1.0, pch = 3)
#' plot(x = tb$datetime, y = tb$aod, pch = 15, cex = 1,
#'      main = paste0("Milepost 97 (", startdate, ")"), 
#'      xlab = "Time", ylab = "AOD")
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
  
  # Format location coordinates
  location <- data.frame(lon = longitude, lat = latitude)
  
  # Format the timestamps
  timeStrings <- attributes(rasterStack)$z$time
  datetime <- lubridate::parse_date_time(timeStrings, orders = "YmdHMS")
  
  # Store the aggregated AOD value of the location in each layer of the stack
  aod <- c()
  for ( i in c(1:length(rasterStack[1])) ) {
    layer <- rasterStack[[i]]
    aod[i] <- raster::extract(x = layer, y = location, 
                              method = method,
                              buffer = buffer,
                              fun = fun)
  }
  
  # Create a tibble with the AOD values and their timestamps
  tb <- tibble::tibble(datetime, aod)
  return(tb)
  
}
