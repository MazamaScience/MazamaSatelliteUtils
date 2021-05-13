#' @export
#' 
#' @title Plot a GOES scan SPDF
#' 
#' @param spdf SpatialPointsDataFrame with an AOD variable.
#' @param bbox Bounding box for the visible region. Defaults to CONUS.
#' @param pointSize Size of plot points. Defaults to 0.5.
#' @param pointShape Shape of the plot points (index). Defaults to 15 (filled 
#' square).
#' @param pointAlpha Transparency of the points. If not explicitly defined, it 
#' will default to 1.0 when \code{includeMap=FALSE} and 0.75 when
#' \code{includeMap=TRUE}.
#' @param paletteColors Vector of colors defining the gradient for the color 
#' legend. Will be ignored if \code{breaks} is set. Defaults to 
#' \code{c("#FFFFB2", "#BD0026")}.
#' @param paletteName Name of an RColorBrewer palette. Defaults to 'YlOrRd'.
#' @param paletteBreaks Vector of AOD values to use as palette breaks.
#' @param legendLimits Upper and lower values for the color legend. Setting this
#'  guarantees that the legend will appear even if the SPDF has nothing but
#'  \code{NA} values. All values outside this range will be set to \code{NA}.
#' @param includeMap Logical flag determining whether to draw a topographic map 
#' image under the points. Since the image is Mercator projected, the plot 
#' coordinate system will be Mercator projected to match. Defaults to FALSE.
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
#' scanSpdf <- goesaodc_createScanSpdf(
#'   filename = scanFile,
#'   bbox = bboxKingcadeFire
#' )
#' 
#' emptyScanSpdf <- goesaodc_createScanSpdf(
#'   filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
#'   bbox = bboxKingcadeFire
#' )
#' 
#' # Plot SPDF for a scan
#' goesaodc_plotScanSpdf(
#'   spdf = scanSpdf,
#'   bbox = bboxKingcadeFire,
#'   paletteBreaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
#'   includeMap = TRUE,
#'   zoom = 8,
#'   stateCodes = "CA",
#'   title = "Kincade fire"
#' )
#' 
#' # Plot SPDF for a scan filled with NA AOD values
#' goesaodc_plotScanSpdf(
#'   spdf = emptyScanSpdf,
#'   bbox = bboxKingcadeFire,
#'   legendLimits = c(-1, 6),
#'   stateCodes = "CA"
#' )
#' }

goesaodc_plotScanSpdf <- function(
  spdf = NULL,
  bbox = bbox_CONUS,
  pointSize = 0.5,
  pointShape = 15,
  pointAlpha = NULL,
  paletteColors = c("#FFFFB2", "#BD0026"),
  paletteName = "YlOrRd",
  paletteBreaks = NULL,
  legendLimits = NULL,
  includeMap = FALSE,
  zoom = NULL,
  stateCodes = NULL,
  title = NULL,
  legendTitle = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !("SpatialPointsDataFrame" %in% class(spdf)) )
    stop("Parameter 'spdf' must be an object of type 'SpatialPointsDataFrame'")
  
  if ( includeMap )
    if ( is.null(zoom) )
      stop("Parameter 'zoom' must be set when including a map layer")
  
  # ----- Prepare for plotting -------------------------------------------------
  
  df <- data.frame(spdf)
  
  pointAlpha <- if ( is.null(pointAlpha) ) {
    ifelse(includeMap, 0.75, 1.0)
  } else {
    pointAlpha
  }
  
  varNames <-
    df %>%
    dplyr::select(-c(lon, lat, optional)) %>%
    names()
  
  legendTitle <- ifelse(is.null(legendTitle), varNames[1], legendTitle)
  
  xlim <- bbox[1:2]
  ylim <- bbox[3:4]
  
  # ----- Create plot layers ---------------------------------------------------
  
  # Create base layer
  baseLayer <- AirFirePlots::plot_base(
    title = title,
    clab = legendTitle,
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
    ggplot2::aes_string(
      x = "lon",
      y = "lat",
      color = varNames[1]
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
    ggplot2::scale_color_gradientn(
      colors = paletteColors,
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
