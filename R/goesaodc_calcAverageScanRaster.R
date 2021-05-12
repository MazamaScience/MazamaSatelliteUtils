#' @export
#' 
#' @title Calculate the average raster of a series of GOES scan rasters
#' 
#' @description Creates a \code{RasterLayer} of averaged AOD readings from a 
#' \code{RasterBrick} of GOES scan \code{RasterLayers}s.
#' 
#' @param rasterBrick A \code{RasterBrick} of GOES scan \code{RasterLayers}s.
#' @param na.rm Logical flag whether to remove NA values before calculating the 
#' average. Defaults to \code{FALSE}.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bboxOregon <- c(-125, -116, 42, 47)
#' 
#' scanFiles <- goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' # Create a raster from scans averaged over a time range
#' scanRasterBrick <- goesaodc_createScanRaster(
#'   filename = scanFiles,
#'   bbox = bboxOregon,
#'   cellSize = 0.05
#' )
#' 
#' goesaodc_calcAverageScanRaster(
#'   rasterBrick = scanRasterBrick,
#'   na.rm = TRUE
#' )
#' }

goesaodc_calcAverageScanRaster <- function(
  rasterBrick = NULL,
  na.rm = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !("RasterBrick" %in% class(rasterBrick)) )
    stop("Parameter 'rasterBrick' must be a 'RasterBrick' object")
  
  if ( dim(rasterBrick)[3] == 0 )
    stop("Parameter 'rasterBrick' must contain at least one 'RasterLayer'")
  
  # ----- Calculate average cell AOD values ------------------------------------
  
  avgRaster <- raster::calc(
    rasterBrick,
    fun = mean,
    na.rm = na.rm
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(avgRaster)
  
}
