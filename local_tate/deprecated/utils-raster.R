#' @keywords utils
#' 
#' @export
#' 
#' @title Get raster value at a point
#' 
#' @description Gets the raster value at a given point.
#' 
#' @param raster A \code{RasterLayer} object.
#' @param x Longitude value (deg E).
#' @param y Latitude value (deg N).
#' 
#' @return Value of raster cell covering the point.

getValue <- function(
  raster,
  x,
  y
) {
  
  value <- raster::getValues(raster)[raster::cellFromXY(raster, c(x, y))]
  return(value)
  
}


#' @export
#' 
#' @title Create an average AOD RasterBrick
#' 
#' @description Creates an average AOD RasterBrick for a given hour. 
#' 
#' @param rasterStack A GOES AOD \code{RasterStack}.
#' @param na.rm Logical that determines if an average should be taken for cells
#' with no values.
#' 
#' @return RasterBrick
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
#' rasterAvg <- raster_createStackAverage(rasterStack, na.rm = TRUE)
#' 
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

raster_createStackAverage <- function(
  rasterStack = NULL,
  na.rm = FALSE
) {
  
  stackAvg <- raster::mean(rasterStack, na.rm = na.rm)
  return(stackAvg)
  
}
