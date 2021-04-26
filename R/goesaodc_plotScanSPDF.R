#' @export
#' 
#' @title Create a plot from a SpatialPointsDataFrame
#' 
#' @description Creates a plot of AOD points defined by a 
#' SpatialPointsDataFrame.
#' 
#' @param sp A SpatialPointsDataFrame.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param pointSize Size of plot points; Defaults to 0.5.
#' @param pointShape Shape of the plot points (index); Defaults to 15 (filled 
#' square).
#' @param breaks Vector of AOD values to use as palette breaks.
#' @param paletteName The name of an RColorBrewer palette; Defaults to 'YlOrRd'.
#' @param title Title of the plot.

goesaodc_plotScanSPDF <- function(
  sp = NULL,
  bbox = bbox_CONUS,
  pointSize = 0.5,
  pointShape = 15,
  breaks = NULL,
  paletteName = "YlOrRd",
  title = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(sp)
  
  if ( !("SpatialPointsDataFrame" %in% class(sp)) )
    stop("Parameter 'sp' must be an object of class 'SpatialPointsDataFrame'")
  
  # ----- Prepare data ---------------------------------------------------------
  
  # Define color scale
  colorScale <- NULL
  if ( is.null(breaks) ) {
    colorScale <- ggplot2::scale_color_gradient(
      low = "#FFFFB2",
      high = "#BD0026",
      na.value = "gray50",
      limits = c(0, 5)
    )
  } else {
    colorScale <- ggplot2::scale_color_stepsn(
      breaks = breaks,
      colors = RColorBrewer::brewer.pal(length(breaks - 1), paletteName),
      limits = c(-1, 6)
    )
  }
  
  # Convert SpatialPointsDataFrame a to regular dataframe
  df <- data.frame(sp)
  
  # ----- Create plot ----------------------------------------------------------
  
  scanPlot <-
    AirFirePlots::plot_base(
      title = title,
      clab = "AOD",
      xlim = bbox[1:2],
      ylim = bbox[3:4],
      project = TRUE,
      expand = FALSE
    ) +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(
        x = .data$lon,
        y = .data$lat,
        color = .data$AOD
      ),
      size = pointSize,
      shape = pointShape
    ) +
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
  
  bbox_oregon <- c(-125, -116, 42, 47)
  
  filename <- "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc"
  
  title <- MazamaCoreUtils::parseDatetime(
    goesaodc_convertFilenameToDatetime(filename),
    "America/Los_Angeles"
  )
  
  # Create spatial points
  sp <- goesaodc_createSingleScanSPDF(
    filename = filename,
    bbox = bbox_oregon,
    dqfLevel = 3
  )
  
  # Plot spatial points with default palette
  goesaodc_plotScanSPDF(
    sp = sp,
    bbox = bbox_oregon,
    title = title
  ) +
    AirFirePlots::layer_states("OR")
  
  # Plot spatial points with custom palette
  goesaodc_plotScanSPDF(
    sp = sp,
    bbox = bbox_oregon,
    breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    paletteName = "Reds",
    title = title
  ) +
    AirFirePlots::layer_states("OR")
  
}
