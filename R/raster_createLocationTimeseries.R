#' @export
#' 
#' @title Create a Tibble for a location's AOD readings at a given hour
#' 
#' @param rasterStack a RasterStack with a RasterLayer for every data snapshot.
#' @param longitude Longitude of the location of interest in decimal degrees E.
#' @param latitude Latitude of the location of interest in decimal degrees N.
#' @param bbox bounding box for the rasters.
#' @param method determines how a value is extracted from the location if a
#' buffer is not used. Can be set to either "simple", which returns the exact 
#' value of the cell the coordinates fall in, or "bilinear", which interpolates 
#' the four nearest raster cell values.
#' @param buffer radius for a circluar region around the location. If multiple
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
#' \dontrun{
#' # Library setup
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusStates")
#' 
#' # Oregon on August 1, 2019 at 12pm (Milepost 97 Fire)
#' datetime <- lubridate::ymd_h("2019-08-01 19", tz = "UTC")
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' bbox_oregon <- sp::bbox(oregon)
#' lon <- -123.245
#' lat <- 42.861
#' 
#' # Gather all the raster layers for the given hour into a stack
#' rasterStack <- goesaodc_createHourlyRasterStack(
#'   satID = "G16",
#'   datetime = datetime, 
#'   bbox = bbox_oregon,
#'   res = 0.05,
#'   dqfLevel = 2
#' )
#' 
#' rasterAvg <- raster::mean(rasterStack, na.rm = TRUE)
#' tb <- raster_createLocationTimeseries(rasterStack = rasterStack,
#'                                       longitude = lon, latitude = lat, 
#'                                       bbox = bbox_oregon,
#'                                       buffer = 1000)
#' 
#' # Plot AOD time series
#' pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
#' par(mfrow = c(1, 2))
#' raster::plot(rasterAvg, main = "Average AOD", col = pal_aod(50),
#'              xlim = c(-125, -122), ylim = c(42, 44))
#' plot(oregon, add = TRUE)
#' points(x = c(lon), y = c(lat), cex = 2.0, pch = 3, lwd = 1.5)
#' plot(x = tb$datetime, y = tb$aod, pch = 15, cex = 1,
#'      main = datetime, xlab = "Time", ylab = "AOD")
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
