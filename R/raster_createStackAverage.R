#' @export
#' 
#' @title Create a RasterBrick Average for a specified hour
#' 
#' @param rasterStack startdate, specified to the hour, in any Y-m-d H format or
#' \code{POSIXct}
#' @param na.rm logical that determines if an average should be taken for pixels
#' that do not always have values
#' 
#' @description Creates a RasterBrick of the average AOD for a given hour. 
#' 
#' @return RasterBrick
#' 
#' @examples
#' \dontrun{
#' # Library setup
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusStates")
#'
#' # Take average for Oregon on July 31, 2019 at 9am (the Milepost 97 Fire)
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' bbox_oregon <- sp::bbox(oregon)
#' datetime <- lubridate::ymd_h("2019-08-01 16", tz = "UTC")
#' 
#' rstrStack <- goesaodc_createHourlyRasterStack(
#'   satID = "G16",
#'   datetime = datetime, 
#'   bbox = bbox_oregon,
#'   res = 0.5,
#'   dqfLevel = 2
#' )
#' avg <- raster_createStackAverage(rstrStack, na.rm = TRUE)
#'
#' # Plot average AOD raster and state border
#' pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
#' title <- paste0("Average AOD (", 
#'                 strftime(datetime, "%Y-%m-%d %I%p", tz = "America/Los_Angeles"), 
#'                 " PDT)")
#' plot(avg, col = pal_aod(50), main = title)
#' plot(oregon, add = TRUE)
#' }

raster_createStackAverage <- function(
  rasterStack = NULL,
  na.rm = FALSE
) {
  
  stackAvg <- raster::mean(rasterStack, na.rm = na.rm)
  return(stackAvg)
  
}
