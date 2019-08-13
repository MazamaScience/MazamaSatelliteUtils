#' @export
#' 
#' @title Create a RasterBrick Average for a specified hour
#' 
#' @param startdate startdate, specified to the hour, in any Y-m-d H format or
#' \code{POSIXct}
#' @param state a SpatialPolygonsDataFrame for the desired state
#' @param res resolution of raster in degrees
#' @param dqfLevel data quality flag level, numeric (0, 1, 2, or 3)
#' @param na.rm logical that determines if an average should be taken for pixels
#' that do not always have values
#' 
#' @description Creates a RasterBrick of a state's average AOD for a given hour. 
#' 
#' Data quality level `dqfLevel` can take a value of:
#' 
#' 0: High quality retrieval flag
#' 1: Medium quality retrieval flag
#' 2: Low quality retrieval flag
#' 3: No retrieval quality flag
#' 
#' @return RasterBrick
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
#' # Take average for Oregon on July 31, 2019 at 9am (the Milepost 97 Fire)
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' date <- lubridate::ymd_h("2019-07-31 16", tz = "UTC")
#' avg <- goesaodc_createHourlyAverageRaster(date, oregon, res = 0.05, na.rm = TRUE)
#'
#' # Plot average AOD raster and state border
#' pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
#' title <- paste0("Average AOD (", 
#'                 strftime(date, "%Y-%m-%d %I%p", tz = "America/Los_Angeles"), 
#'                 " PDT)")
#' plot(avg, col = pal_aod(50), main = title)
#' plot(oregon, add = TRUE)
#' }

goesaodc_createHourlyAverageRaster <- function(
  startdate = NULL,
  state = NULL, 
  res = 0.1,
  dqfLevel = NULL,
  na.rm = TRUE
) {
  
  state_bbox <- sp::bbox(state)
  rstrStack <- goesaodc_createHourlyRasterStack(startdate = startdate, 
                                                bbox = state_bbox,
                                                res = res, 
                                                dqfLevel = dqfLevel)
  stackAvg <- mean(rstrStack, na.rm = na.rm)
  return(stackAvg)
  
}
