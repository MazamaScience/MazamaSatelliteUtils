#' @export
#' 
#' @title Create a plot from a SpatialPointsDataFrame
#' 
#' @description Creates a plot of AOD points defined by a 
#' SpatialPointsDataFrame.
#' 
#' @param sp A SpatialPointsDataFrame.
#' @param pointSize Size of plot points; Defaults to 0.5.
#' @param breaks Vector of AOD values to use as palette breaks.
#' @param paletteName The name of an RColorBrewer palette; Defaults to 'YlOrRd'.
#' @param title Title of the plot.

goesaodc_SPDFToPlot <- function(
  sp = NULL,
  pointSize = 0.5,
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
      low = "#fff5d9",
      high = "#ab2626",
      na.value = "gray50"
    )
  } else {
    colorScale <- ggplot2::scale_color_stepsn(
      breaks = breaks,
      colors = RColorBrewer::brewer.pal(length(breaks - 1), paletteName)
    )
  }
  
  # convert SpatialPointsDataFrame to regular dataframe
  df <- data.frame(sp)
  
  # ----- Create plot ----------------------------------------------------------
  
  p <-
    AirFirePlots::plot_base(
      title = "AOD for 2020-09-08 17:31:17 PDT",
      clab = "AOD",
      xlim = oregon_bbox[1:2],
      ylim = oregon_bbox[3:4],
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
      shape = 15
    ) +
    colorScale
  
  # ----- Return ---------------------------------------------------------------
  
  return(p)
  
}

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  
  loadSpatialData("NaturalEarthAdm1")
  
  oregon_bbox <- c(-125, -116, 42, 47)
  
  filename <- "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc"
  
  # Create spatial points
  sp <- goesaodc_singleScanToSPDF(
    filename = filename,
    bbox = oregon_bbox,
    dqfLevel = 3
  ) 
  
  # Plot spatial points
  goesaodc_SPDFToPlot(
    sp = sp,
    breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    title = goesaodc_convertFilenameToDatetime(filename)
  ) +
    AirFirePlots::layer_states(
      stateCodes = "OR"
    )
  
}
