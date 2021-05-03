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
#' @param paletteName The name of an RColorBrewer palette. Defaults to 'YlOrRd'.
#' @param breaks Vector of AOD values to use as palette breaks.
#' @param limits Upper and lower AOD values to use as color scale bounds. 
#' Setting this guarantees that the color legend is displayed even if the scan 
#' has nothing but NA AOD values.
#' @param rasterAlpha Alpha value of the raster. Defaults to 0.75.
#' @param includeMap Logical flag to draw a topographic map image under the 
#' raster. Since the image is Mercator projected, the plot coordinate system 
#' will be Mercator projected to match. This significantly slows down the 
#' drawing of rasters. Defaults to FALSE.
#' @param zoom Zoom level of the topographic map, if it is included. Must be an
#' integer from 1 to 15.
#' @param stateCodes Codes of state outlines to draw.
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
  paletteName = "YlOrRd",
  breaks = NULL,
  limits = NULL,
  rasterAlpha = 0.75,
  includeMap = FALSE,
  zoom = NULL,
  stateCodes = NULL,
  title = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( includeMap )
    if ( is.null(zoom) )
      stop("Parameter 'zoom' must be set when including a map layer")
  
  # ----- Create raster --------------------------------------------------------
  
  raster <- goesaodc_createScanRaster(
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
  
  # ----- Create plot layers ---------------------------------------------------
  
  xlim <- bbox[1:2]
  ylim <- bbox[3:4]
  
  # Create base layer
  baseLayer <- AirFirePlots::plot_base(
    title = title,
    flab = "AOD",
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
  fillScale <- if ( is.null(breaks) ) {
    ggplot2::scale_fill_gradient(
      low = "#FFFFB2",
      high = "#BD0026",
      na.value = "gray50",
      limits = limits
    )
  } else {
    ggplot2::scale_fill_stepsn(
      breaks = breaks,
      colors = RColorBrewer::brewer.pal(
        length(breaks - 1),
        paletteName
      ),
      na.value = "gray50",
      limits = limits
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

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  
  loadSpatialData("NaturalEarthAdm1")
  
  bbox_oregon <- c(-125, -116, 42, 46.5)
  
  # Plot raster for a scan specified by satellite + time info
  goesaodc_plotScanRaster(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    cellSize = 0.05,
    stateCodes = "OR",
    title = "Oregon AOD at 5:30pm PDT on Sep. 8, 2020"
  )
  
  # Plot raster for a scan specified by file name
  goesaodc_plotScanRaster(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
    bbox = bbox_oregon,
    cellSize = 0.05,
    breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    stateCodes = "OR"
  )
  
  # Plot raster points for a range of scans
  goesaodc_plotScanRaster(
    satID = "G17",
    datetime = "2020-09-08 12",
    endtime = "2020-09-08 13",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    cellSize = 0.05,
    stateCodes = "OR",
    title = "Oregon AOD from 12pm to 1pm PDT on Sept. 8, 2020"
  )
  
  # Plot raster for a scan with NA AOD values
  goesaodc_plotScanRaster(
    satID = "G17",
    datetime = "2019-10-27 10:00",
    timezone = "America/Los_Angeles",
    bbox = c(-124, -120, 36, 39),
    cellSize = 0.05,
    rasterAlpha = 0.6,
    includeMap = TRUE,
    zoom = 7
  )
  
  # Plot average raster for a range of scans with NA AOD values
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2019-10-27 10:00",
    endtime = "2019-10-27 11:00",
    timezone = "America/Los_Angeles",
    bbox = c(-124, -120, 36, 39),
    stateCodes = "CA",
    title = "San Francisco AOD from 10am to 11am on Oct. 27, 2019"
  )
  
  # Plot raster for a faulty scan
  goesaodc_plotScanRaster(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
    bbox = bbox_oregon,
    cellSize = 0.05,
    stateCodes = "OR",
    breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    limits = c(-1, 6)
  )
  
  # Plot raster for a non-existent scan
  goesaodc_plotScanRaster(
    satID = "G17",
    datetime = "1970-01-01 12:00",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    cellSize = 0.05
  )
  
}