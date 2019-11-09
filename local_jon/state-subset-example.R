#
# Single state plot
#

if ( FALSE ) {
  
  library(sp)
  library(MazamaSpatialUtils)
  setSpatialDataDir("~/Data/Spatial")
  loadSpatialData("USCensusStates")
  
  library(MazamaSatelliteUtils)
  setSatelliteDataDir("~/Data/Satellite")
  
  netCDF <- system.file("extdata",
                        "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc",
                        package = "MazamaSatelliteUtils")
  nc <- goesaodc_openFile(netCDF)
  
}


stateSubsetExample <- function(
  nc = NULL,
  stateCode = "CA",
  dqfLevel = 2,
  exponentiate = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(stateCode)
  
  if ( is.null(exponentiate) || !is.logical(exponentiate) ) 
    exponentiate <- TRUE
  
  # ----- Prepare data ---------------------------------------------------------
  
  # Subset SpatialPolygonsDataFrame and get bbox
  renamedCode <- stateCode
  stateSPDF <- subset(USCensusStates, stateCode == renamedCode)
  bbox <- sp::bbox(stateSPDF)
  
  # Load data within bbox and create SpatialPoints
  result <- try({
    SP <- goesaodc_createSpatialPoints(nc, bbox, dqfLevel)
  }, silent = TRUE)
  
  noData <- FALSE
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "No data for selected region") ) {
      noData <- TRUE
    } else {
      stop(err_msg)
    }
  }
  
  if ( !noData && exponentiate ) {
    SP@data$AOD <- 10^SP@data$AOD
  }
  
  # ----- Plot style -----------------------------------------------------------
  
  if ( exponentiate ) {
    
    n <- 5e5
    breaks <- c(0,5,10,20,40,Inf)
    colBins <- 5
    paletteName <- "YlOrRd"
    pch <- 15
    cex <- 0.5
    
  } else {
    
    # TODO:  What is better than default settings?
    n <- 1e5
    breaks <- NULL
    colBins <- 5
    paletteName <- "YlOrRd"
    pch <- 15
    cex <- 0.5
    
  }
  
  # ----- Create plot ----------------------------------------------------------
  
  plot(stateSPDF)
  
  usr <- par('usr')
  graphics::rect(usr[1], usr[3], usr[2], usr[4], col = "gray80")
  
  if ( noData ) {
    
    x <- (usr[2] - usr[1])/2
    graphics::text(x = usr[1] + (usr[2] - usr[1])/2,
                   y = usr[3] + (usr[4] - usr[3])/2,
                   sprintf("No Data Found for dqfLevel %d", dqfLevel))
    
  } else {
    
    goesaodc_plotSpatialPoints(
      SP, 
      var = "AOD",
      n = n,
      colBins = colBins,
      breaks = breaks,
      paletteName = paletteName,
      pch = pch,
      cex = cex,
      add = TRUE
    )
    
  }
  
  plot(stateSPDF, add = TRUE)
  
  # TODO:  Add title and other annotations
  
  
}