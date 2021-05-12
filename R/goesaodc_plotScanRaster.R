#' @export
#' 
#' @title Plot GOES AOD rasters 
#' 
#' @param raster A RasterBrick with an AOD variable.
#' @param bbox Bounding box for the region of interest. Defaults to CONUS.
#' @param fun Function used to summarize multiple point values within a single 
#' cell; Defaults to \code{mean}.
#' @param rasterAlpha Transparency of the raster. If not explicitly defined, it 
#' will default to 1.0 when \code{includeMap=FALSE} and 0.75 when
#' \code{includeMap=TRUE}.
#' @param paletteColors Vector of colors to use as a gradient for the color 
#' legend. Will be ignored if \code{breaks} is set. Defaults to 
#' \code{c("#FFFFB2", "#BD0026")}.
#' @param paletteName The name of an RColorBrewer palette. Defaults to 'YlOrRd'.
#' @param paletteBreaks Vector of AOD values to use as palette breaks.
#' @param legendLimits Upper and lower AOD values for the fill legend. Setting 
#' this guarantees that the legend is displayed even if the scan has nothing but
#' NA AOD values. All values outside the range will be set to NA.
#' @param includeMap Logical flag to draw a topographic map image under the 
#' raster. Since the image is Mercator projected, the plot coordinate system 
#' will be Mercator projected to match. This significantly slows down the 
#' drawing of rasters. Defaults to FALSE.
#' @param zoom Zoom level of the topographic map, if it is included. Must be an
#' integer from 1 to 15.
#' @param stateCodes Codes of state outlines to draw.
#' @param title Title of the plot.
#' @param legendTitle Title of the plot legend.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' loadSpatialData("NaturalEarthAdm1")
#' 
#' bboxKingcadeFire <- c(-124, -120, 36, 39)
#' 
#' scanFile <- goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2019-10-27 10:00",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' scanRaster <- goesaodc_createScanRaster(
#'   filename = scanFile,
#'   bbox = bboxKingcadeFire,
#'   cellSize = 0.05
#' )
#' 
#' faultyScanRaster <- goesaodc_createScanRaster(
#'   filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
#'   bbox = bboxKingcadeFire,
#'   cellSize = 0.05
#' )
#' 
#' # Plot a raster for a scan
#' goesaodc_plotScanRaster(
#'   raster = scanRaster,
#'   bbox = bboxKingcadeFire,
#'   paletteBreaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
#'   includeMap = TRUE,
#'   zoom = 8,
#'   stateCodes = "CA",
#'   title = "Kincade fire"
#' )
#' 
#' # Plot a raster for a scan filled with NA AOD values
#' goesaodc_plotScanRaster(
#'   raster = faultyScanRaster,
#'   bbox = bboxKingcadeFire,
#'   legendLimits = c(-1, 6),
#'   stateCodes = "CA"
#' )
#' }

goesaodc_plotScanRaster <- function(
  raster = NULL,
  bbox = bbox_CONUS,
  fun = mean,
  rasterAlpha = NULL,
  paletteColors = c("#FFFFB2", "#BD0026"),
  paletteName = "YlOrRd",
  paletteBreaks = NULL,
  legendLimits = NULL,
  includeMap = FALSE,
  zoom = NULL,
  stateCodes = NULL,
  title = NULL,
  legendTitle = "AOD"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !("RasterLayer" %in% class(raster)) )
    stop("Parameter 'raster' must be an object of type 'RasterLayer'")
  
  if ( includeMap )
    if ( is.null(zoom) )
      stop("Parameter 'zoom' must be set when including a map layer")
  
  rasterAlpha <- if ( is.null(rasterAlpha) ) {
    ifelse(includeMap, 0.75, 1.0)
  } else {
    rasterAlpha
  }
  
  # ----- Create plot layers ---------------------------------------------------
  
  xlim <- bbox[1:2]
  ylim <- bbox[3:4]
  
  # Create base layer
  baseLayer <- AirFirePlots::plot_base(
    title = title,
    flab = legendTitle,
    xlim = xlim,
    ylim = ylim,
    project = includeMap,
    expand = FALSE
  )
  
  # Create map layer
  mapLayer <- if ( includeMap ) {
    AirFirePlots::layer_map(
      zoom = zoom,
      xlim = xlim,
      ylim = ylim
    )
  } else {
    NULL
  }
  
  # Create raster layer
  rasterLayer <- AirFirePlots::layer_raster(
    raster = raster,
    varName = "AOD",
    alpha = rasterAlpha
  )
  
  # Create states layer
  statesLayer <- if ( is.null(stateCodes) ) {
    NULL
  } else {
    AirFirePlots::layer_states(stateCodes)
  }
  
  # Create fill scale
  fillScale <- if ( is.null(paletteBreaks) ) {
    ggplot2::scale_fill_gradientn(
      colors = paletteColors,
      na.value = "gray50",
      limits = legendLimits
    )
  } else {
    ggplot2::scale_fill_stepsn(
      breaks = paletteBreaks,
      colors = RColorBrewer::brewer.pal(
        length(paletteBreaks) - 1,
        paletteName
      ),
      na.value = "gray50",
      limits = legendLimits
    )
  }
  
  # ----- Create plot ----------------------------------------------------------
  
  scanPlot <-
    baseLayer +
    mapLayer +
    rasterLayer +
    statesLayer +
    fillScale
  
  # ----- Return ---------------------------------------------------------------
  
  return(scanPlot)
  
}
