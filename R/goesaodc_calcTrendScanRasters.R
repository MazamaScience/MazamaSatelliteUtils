#' @export
#' 
#' @title Calculate the trend of a series of GOES scan rasters
#' 
#' @description Creates a \code{SpatialPointsDataFrame} of AOD reading trends 
#' from a GOES scan \code{rasterBrick}. A trend value for a cell is calculated 
#' by taking the difference between it's average value in the first half of the 
#' scan series and it's average value in the second.
#' 
#' @param rasterBrick A \code{RasterBrick} of GOES scan \code{RasterLayers}s.
#' @param na.rm Logical flag whether to remove NA values before calculating the
#' trend. Defaults to \code{FALSE}.
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
#' scanRasterBrick <- goesaodc_createScanRaster(
#'   filename = scanFiles,
#'   bbox = bboxOregon,
#'   cellSize = 0.05
#' )
#' 
#' goesaodc_calcTrendScanRaster(
#'   rasterBrick = scanRasterBrick,
#'   na.rm = TRUE
#' )
#' }

goesaodc_calcTrendScanRaster <- function(
  rasterBrick = NULL,
  na.rm = FALSE
) {
  
  middleScanIndex <- floor(dim(rasterBrick)[3] / 2)
  
  half1Avg <- goesaodc_calcAverageScanRaster(
    raster::subset(rasterBrick, 1:middleScanIndex),
    na.rm = na.rm
  )[]
  
  half2Avg <- goesaodc_calcAverageScanRaster(
    raster::subset(rasterBrick, (middleScanIndex + 1):dim(rasterBrick)[3]),
    na.rm = na.rm
  )[]
  
  trendValues <- 
    tibble::tibble(half1Avg, half2Avg) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(trend = .data$half2Avg - .data$half1Avg) %>%
    dplyr::pull(.data$trend)
  
  trendRaster <- raster::raster(rasterBrick)
  values(trendRaster) <- trendValues
  
  return(trendRaster)
  
}
