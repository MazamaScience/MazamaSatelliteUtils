#' @export
#' 
#' @title Create a plot from a SpatialPointsDataFrame
#' 
#' @description Creates a plot of AOD points defined by a 
#' SpatialPointsDataFrame.
#' 
#' @param spdf A SpatialPointsDataFrame.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param pointSize Size of plot points; Defaults to 0.5.
#' @param pointShape Shape of the plot points (index); Defaults to 15 (filled 
#' square).
#' @param breaks Vector of AOD values to use as palette breaks.
#' @param paletteName The name of an RColorBrewer palette; Defaults to 'YlOrRd'.
#' @param title Title of the plot.

goesaodc_plotScanSPDF <- function(
  spdf = NULL,
  bbox = bbox_CONUS,
  pointSize = 0.5,
  pointShape = 15,
  breaks = NULL,
  paletteName = "YlOrRd",
  title = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(spdf)
  
  if ( !("SpatialPointsDataFrame" %in% class(spdf)) )
    stop("Parameter 'spdf' must be an object of class 'SpatialPointsDataFrame'")
  
  # ----- Create plot layers ---------------------------------------------------
  
  # Create points layer
  if ( nrow(spdf@data) > 0 ) {
    
    df <- data.frame(spdf)
    
    pointsLayer <- ggplot2::geom_point(
      data = df,
      ggplot2::aes(
        x = .data$lon,
        y = .data$lat,
        color = .data$AOD
      ),
      size = pointSize,
      shape = pointShape
    )
    
  } else {
    
    # Define dummy data
    df <- data.frame(
      lon = 0,
      lat = 0,
      AOD = 0
    )
    
    # Create invisible points layer. The aesthetics must still be set so the 
    # plot axes and legend to be drawn.
    pointsLayer <- ggplot2::geom_point(
      data = df,
      ggplot2::aes(
        x = .data$lon,
        y = .data$lat,
        color = .data$AOD 
      ),
      size = 0
    )
    
  }
  
  # Create color scale
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
    pointsLayer +
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
  
  title <- 
    filename %>%
    goesaodc_convertFilenameToDatetime() %>%
    MazamaCoreUtils::parseDatetime(timezone = "America/Los_Angeles")
  
  # Create spatial points
  spdf <- goesaodc_createSingleScanSPDF(
    filename = filename,
    bbox = bbox_oregon
  )
  
  # Plot spatial points with default palette
  goesaodc_plotScanSPDF(
    spdf = spdf,
    bbox = bbox_oregon,
    title = title
  ) +
    AirFirePlots::layer_states("OR")
  
  # Plot spatial points with custom palette
  goesaodc_plotScanSPDF(
    spdf = spdf,
    bbox = bbox_oregon,
    breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    paletteName = "Reds",
    title = title
  ) +
    AirFirePlots::layer_states("OR")
  
}
