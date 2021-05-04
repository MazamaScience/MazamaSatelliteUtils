#' @export
#' 
#' @title Plot GOES AOD scan points
#' 
#' @param spdf A SpatialPointsDataFrame with AOD and DQF variables.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Allowed data quality level. All readings with a DQF value
#' above this level will have their AOD values set to NA. Must be either 0, 1, 
#' 2, or 3, with 0 being the highest quality. Defaults to 3.
#' @param pointSize Size of plot points; Defaults to 0.5.
#' @param pointShape Shape of the plot points (index); Defaults to 15 (filled 
#' square).
#' @param pointAlpha Transparency of the points If not explicitly defined, it 
#' will default to 1.0 when \code{includeMap=FALSE} and 0.75 when
#' \code{includeMap=TRUE}.
#' @param paletteName The name of an RColorBrewer palette; Defaults to 'YlOrRd'.
#' @param paletteBreaks Vector of AOD values to use as palette breaks.
#' @param legendLimits Upper and lower AOD values for the color legend. Setting 
#' this guarantees that the legend is displayed even if the scan has nothing but
#' NA AOD values. All values outside the range will be set to NA.
#' @param includeMap Logical flag to draw a topographic map image under the 
#' raster. Since the image is Mercator projected, the plot coordinate system 
#' will be Mercator projected to match. Defaults to FALSE.
#' @param zoom Zoom level of the topographic map, if it is included. Must be an
#' integer from 1 to 15.
#' @param stateCodes Codes of state outlines to draw.
#' @param title Title of the plot.

goesaodc_plotScanPoints <- function(
  spdf = NULL,
  bbox = bbox_CONUS,
  pointSize = 0.5,
  pointShape = 15,
  pointAlpha = NULL,
  paletteName = "YlOrRd",
  paletteBreaks = NULL,
  legendLimits = NULL,
  includeMap = FALSE,
  zoom = NULL,
  stateCodes = NULL,
  title = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !("SpatialPointsDataFrame" %in% class(spdf)) )
    stop("Parameter 'spdf' must be an object of type 'SpatialPointsDataFrame'")
  
  if ( !all(c("AOD", "DQF") %in% names(spdf)))
    stop("Parameter 'spdf' must have 'AOD' and 'DQF' variables")
  
  if ( includeMap )
    if ( is.null(zoom) )
      stop("Parameter 'zoom' must be set when including a map layer")
  
  pointAlpha <- if ( is.null(pointAlpha) ) {
    ifelse(includeMap, 0.75, 1.0)
  } else {
    pointAlpha
  }
  
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
    data = data.frame(spdf), 
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
  colorScale <- if ( is.null(paletteBreaks) ) {
    
    ggplot2::scale_color_gradient(
      low = "#FFFFB2",
      high = "#BD0026",
      na.value = "gray50",
      limits = legendLimits
    )
    
  } else {
    
    ggplot2::scale_color_stepsn(
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
  
  bboxKingcadeFire <- c(-124, -120, 36, 39)
  
  scanPoints <- goesaodc_createScanPoints(
    satID = "G17",
    datetime = "2019-10-27 10:00",
    timezone = "America/Los_Angeles",
    bbox = bboxKingcadeFire
  )
  
  faultyScanPoints <- goesaodc_createScanPoints(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
    bbox = bboxKingcadeFire
  )
  
  # Plot points for a scan
  goesaodc_plotScanPoints(
    spdf = scanPoints,
    bbox = bboxKingcadeFire,
    paletteBreaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    includeMap = TRUE,
    zoom = 8,
    stateCodes = "CA",
    title = "Kincade fire"
  )
  
  # Plot points for a scan filled with NA AOD values
  goesaodc_plotScanPoints(
    spdf = faultyScanPoints,
    bbox = bboxKingcadeFire,
    legendLimits = c(-1, 6),
    stateCodes = "CA"
  )
  
}
