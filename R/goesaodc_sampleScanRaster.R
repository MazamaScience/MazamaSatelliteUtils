#' @export
#'
#' @title Sample values from a scan raster
#'
#' @param raster \code{RasterLayer} with a variable.
#' @param lon Longitude position.
#' @param lat Latitude position.
#' @param radius Radius of a buffer around each point (in meters) from which to
#' extract cell values. If the distance between the sampling point and the
#' center of a cell is less than or equal to the buffer, the cell is included.
#' @param method Method of extracting cell values. If 'simple' values for the
#' cell a point falls in are returned. If 'bilinear' the returned values are
#' interpolated from the values of the four nearest raster cells. Defaults to
#' 'simple'.
#' @param fun Function used to summarize cell values if multiple cells fall
#' within the given radius. Defaults to `mean`.
#' @param na.rm Logical flag determining whether to remove \code{NA} values
#' before summarizing them with `fun`. Defaults to \code{FALSE}.
#'
#' @return Numeric value extracted from the raster.
#'
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' bboxOregon <- c(-125, -116, 42, 46.5)
#'
#' scanFile <- goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   timezone = "America/Los_Angeles"
#' )
#'
#' # Create a raster for a scan file
#' scanRaster <- goesaodc_createScanRaster(
#'   filename = scanFile,
#'   bbox = bboxOregon,
#'   cellSize = 0.05
#' )
#'
#' goesaodc_plotScanRaster(
#'   scanRaster,
#'   bbox = bboxOregon
#' )
#'
#' goesaodc_sampleScanRaster(
#'   raster = scanRaster,
#'   lon = -123,
#'   lat = 46,
#'   radius = 10000
#' )
#' }

goesaodc_sampleScanRaster <- function(
  raster = NULL,
  lon = NULL,
  lat = NULL,
  radius = 1,
  method = "simple",
  fun = mean,
  na.rm = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(raster)
  MazamaCoreUtils::stopIfNull(lon)
  MazamaCoreUtils::stopIfNull(lat)
  MazamaCoreUtils::stopIfNull(radius)
  MazamaCoreUtils::stopIfNull(method)
  MazamaCoreUtils::stopIfNull(fun)
  ###MazamaCoreUtils::stopIfNull(na.rm) # TODO:  Not currently used

  if ( !("RasterLayer" %in% class(raster)) )
    stop("Parameter 'raster' must be an object of type 'RasterLayer'")

  # ----- Extract value --------------------------------------------------------

  value <- raster::extract(
    x = raster,
    y = data.frame(lon = lon, lat = lat),
    method = method,
    buffer = radius,
    fun = fun,
    na.rm = na.rm
  )

  # ----- Return ---------------------------------------------------------------

  return(value)

}
