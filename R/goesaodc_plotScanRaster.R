#' @export
#' 
#' @title Create raster plots of AOD data 
#' 
#' @description Creates a raster plot of AOD data from a GOES scan. If no 
#' \code{endtime} is given, then only the scan closest to \code{datetime} will
#' be used. If an \code{endtime} is provided, then the plotted raster will be
#' the average of the scans in the given time range. If \code{filename} is 
#' given, just the raster for that scan file will be drawn.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param endtime End time in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}. Defaults to UTC.
#' @param filename The name of the scan file.
#' @param bbox Bounding box for the region of interest. Defaults to CONUS.
#' @param dqfLevel Allowed data quality level. All readings with a DQF value
#' above this level will have their AOD values set to NA. Must be either 0, 1, 
#' 2, or 3, with 0 being the highest quality. Defaults to 3.
#' @param cellSize Size of Raster cells measured in degrees lon/lat.
#' @param fun Function used to summarize multiple point values within a single 
#' cell; Defaults to \code{mean}.
#' @param includeMap Logical flag to draw a topographic map under the raster.
#' Defaults to FALSE.
#' @param zoom Zoom level of the topographic map, if it is included. Must be an
#' integer from 1 to 15.
#' @param stateCodes Codes of state outlines to draw.
#' @param breaks Vector of AOD values to use as palette breaks.
#' @param paletteName The name of an RColorBrewer palette. Defaults to 'YlOrRd'.
#' @param rasterAlpha Alpha value of the raster. Defaults to 0.75.
#' @param title Title of the plot.

goesaodc_plotScanRaster <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3,
  cellSize = NULL,
  fun = mean,
  includeMap = TRUE,
  zoom = NULL,
  stateCodes = NULL,
  breaks = NULL,
  paletteName = "YlOrRd",
  rasterAlpha = 0.75,
  title = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( includeMap )
    if ( is.null(zoom) )
      stop("Parameter 'zoom' must be set when including a map layer")
  
  # ----- Create raster --------------------------------------------------------
  
  scanRaster <- goesaodc_createScanRaster(
    satID = satID,
    datetime = datetime,
    endtime = endtime,
    timezone = timezone,
    filename = filename,
    bbox = bbox,
    dqfLevel = dqfLevel,
    cellSize = cellSize,
    fun = fun
  )
  
  # ----- Plot raster ----------------------------------------------------------
  
  xlim <- bbox[1:2]
  ylim <- bbox[3:4]
  
  # Create base layer
  baseLayer <- AirFirePlots::plot_base(
    title = title,
    flab = "AOD",
    xlim = xlim,
    ylim = ylim,
    project = FALSE,
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
    raster = scanRaster,
    varName = "AOD",
    alpha = rasterAlpha
  )
  
  # Create state borders layer
  stateLayer <- if ( is.null(stateCodes) ) {
    NULL
  } else {
    AirFirePlots::layer_states(stateCodes)
  }
  
  # Create color scale
  if ( is.null(breaks) ) {
    colorScale <- ggplot2::scale_fill_gradient(
      low = "#FFFFB2",
      high = "#BD0026",
      na.value = "gray50",
      limits = c(0, 5)
    )
  } else {
    colorScale <- ggplot2::scale_fill_stepsn(
      breaks = breaks,
      colors = RColorBrewer::brewer.pal(length(breaks - 1), paletteName),
      limits = c(-1, 6)
    )
  }
  
  scanPlot <-
    baseLayer +
    mapLayer +
    rasterLayer +
    stateLayer +
    colorScale
  
  # ----- Return ---------------------------------------------------------------
  
  return(scanPlot)
  
}

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  
  loadSpatialData("NaturalEarthAdm1")
  
  # Plot a scan with NA AOD values
  goesaodc_plotScanRaster(
    satID = "G17",
    datetime = "2019-10-27 10:00",
    timezone = "America/Los_Angeles",
    bbox = c(-124, -120, 36, 39),
    cellSize = 0.03,
    rasterAlpha = 0.6,
    includeMap = TRUE,
    zoom = 7
  )
  
}