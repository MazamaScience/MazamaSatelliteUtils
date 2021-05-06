#' @export
#' 
#' @title Aggregate GOES scan rasters
#' 
#' @description Creates a \code{RasterLayer} of aggregated AOD readings from a 
#' \code{RasterBrick} of GOES scan \code{RasterLayers}s.
#' 
#' @param rasterBrick A \code{RasterBrick} of GOES scan \code{RasterLayers}s.
#' @param fun The function to use for aggregating AOD values. Defaults to 
#' \code{mean}.
#' @param na.rm Logical flag whether to remove NA values before performing the
#' \code{fun} function. Defaults to \code{FALSE}.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bboxOregon <- c(-125, -116, 42, 47)
#' 
#' # Create a raster from scans averaged over a time range
#' scanRasterBrick <- goesaodc_createScanRaster(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles",
#'   bbox = bboxOregon,
#'   cellSize = 0.05
#' )
#' 
#' goesaodc_aggregateScanRasters(
#'   rasterBrick = scanRasterBrick,
#'   na.rm = TRUE
#' )
#' }

goesaodc_aggregateScanRasters <- function(
  rasterBrick = NULL,
  fun = mean,
  na.rm = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !("RasterBrick" %in% class(rasterBrick)) )
    stop("Parameter 'rasterBrick' must be a 'RasterBrick' object")
  
  if ( dim(rasterBrick)[3] == 0 )
    stop("Parameter 'rasterBrick' must contain at least one 'RasterLayer'")
  
  # ----- Aggregate cell AOD values --------------------------------------------
  
  aggregateRaster <- raster::calc(
    rasterBrick,
    fun = mean,
    na.rm = na.rm
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(aggregateRaster)
  
}
