#' @export
#' 
#' @title Plot a GOES AOD scan as points
#' 
#' @description Draws a GOES AOD scan as a plot of points. A scan can be 
#' specified in the following ways:
#' \itemize{
#'  \item{Satellite + time info: Set by \code{satID} and \code{datetime}. If no 
#'    \code{endtime} is given, then only the scan closest to \code{datetime} 
#'    will be used. If an \code{endtime} is provided, then the points plotted
#'    will be the average of the all scans in the given time range.}
#'  \item{File name: Set by \code{filename}.}
#' }
#' A file name takes precedence over satellite + time info if both are given.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param endtime End time in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param filename Scan file name.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Allowed data quality level. All readings with a DQF value
#' above this level will have their AOD values set to NA. Must be either 0, 1, 
#' 2, or 3, with 0 being the highest quality. Defaults to 3.
#' @param pointSize Size of plot points; Defaults to 0.5.
#' @param pointShape Shape of the plot points (index); Defaults to 15 (filled 
#' square).
#' @param paletteName The name of an RColorBrewer palette; Defaults to 'YlOrRd'.
#' @param breaks Vector of AOD values to use as palette breaks.
#' @param limits Upper and lower AOD values to use as color scale bounds. 
#' Setting this guarantees that the color legend is displayed even if the scan 
#' has nothing but NA AOD values.
#' @param pointAlpha Alpha value of the points. Defaults to 0.75.
#' @param includeMap Logical flag to draw a topographic map image under the 
#' raster. Since the image is Mercator projected, the plot coordinate system 
#' will be Mercator projected to match. Defaults to FALSE.
#' @param zoom Zoom level of the topographic map, if it is included. Must be an
#' integer from 1 to 15.
#' @param stateCodes Codes of state outlines to draw.
#' @param title Title of the plot.

goesaodc_plotScanPoints <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3,
  pointSize = 0.5,
  pointShape = 15,
  paletteName = "YlOrRd",
  breaks = NULL,
  limits = NULL,
  pointAlpha = 0.75,
  includeMap = FALSE,
  zoom = NULL,
  stateCodes = NULL,
  title = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( includeMap )
    if ( is.null(zoom) )
      stop("Parameter 'zoom' must be set when including a map layer")
  
  # ----- Create spatial points ------------------------------------------------
  
  spdf <- goesaodc_createScanSPDF(
    satID = satID,
    datetime = datetime,
    endtime = endtime,
    timezone = timezone,
    filename = filename,
    bbox = bbox,
    dqfLevel = dqfLevel
  )
  
  # ggplot2::geom_points() cannot take in a raw SpatialPointsDataFrame
  df <- data.frame(spdf)
  
  # ----- Create plot layers ---------------------------------------------------
  
  xlim <- bbox[1:2]
  ylim <- bbox[3:4]
  
  # Create base layer
  baseLayer <- AirFirePlots::plot_base(
    title = title,
    clab = "AOD",
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
  
  # Create points layer
  pointsLayer <- ggplot2::geom_point(
    data = df,
    ggplot2::aes(
      x = .data$lon,
      y = .data$lat,
      color = .data$AOD
    ),
    size = pointSize,
    shape = pointShape,
    alpha = pointAlpha
  )
  
  # Create states layer
  statesLayer <- if ( is.null(stateCodes) ) {
    NULL
  } else {
    AirFirePlots::layer_states(stateCodes)
  }
  
  # Create color scale
  colorScale <- if ( is.null(breaks) ) {
    ggplot2::scale_color_gradient(
      low = "#FFFFB2",
      high = "#BD0026",
      na.value = "gray50",
      limits = limits
    )
  } else {
    ggplot2::scale_color_stepsn(
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
    pointsLayer +
    statesLayer + 
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
  
  bbox_oregon <- c(-125, -116, 42, 46.5)
  
  # Plot points for a scan specified by satellite + time info
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    stateCodes = "OR",
    title = "Oregon AOD at 5:30pm PDT on Sep. 8, 2020"
  )
  
  # Plot points for a scan specified by file name
  goesaodc_plotScanPoints(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
    bbox = bbox_oregon,
    breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    stateCodes = "OR",
    includeMap = TRUE,
    zoom = 7
  )
  
  # Plot average points for a range of scans
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2020-09-08 12",
    endtime = "2020-09-08 13",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    stateCodes = "OR",
    title = "Oregon AOD from 12pm to 1pm PDT on Sept. 8, 2020"
  )
  
  # Plot points for a scan with NA AOD values
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2019-10-27 10:00",
    timezone = "America/Los_Angeles",
    bbox = c(-124, -120, 36, 39)
  )
  
  # Plot average points for a range of scans with NA AOD values
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2019-10-27 10:00",
    endtime = "2019-10-27 11:00",
    timezone = "America/Los_Angeles",
    bbox = c(-124, -120, 36, 39),
    stateCodes = "CA",
    title = "San Francisco AOD from 10am to 11am on Oct. 27, 2019"
  )
  
  # Plot points for a faulty scan
  goesaodc_plotScanPoints(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
    bbox = bbox_oregon,
    stateCodes = "OR"
  )
  
  # Plot points for a non-existent scan
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "1970-01-01 12:00",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon
  )
  
}
