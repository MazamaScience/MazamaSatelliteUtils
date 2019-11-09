#
# Bbox plot
#

if ( FALSE ) {
  
  library(sp)
  library(MazamaSpatialUtils)
  setSpatialDataDir("~/Data/Spatial")
  loadSpatialData("USCensusStates")
  
  library(MazamaSatelliteUtils)
  setSatelliteDataDir("~/Data/Satellite")
  
  files <- goesaodc_downloadAOD("G17", 2019102714, timezone = "America/Los_Angeles")
  files <- goesaodc_listFiles("G17", 2019102714, timezone = "America/Los_Angeles")
  
  # Kincade fire
  bbox <- c(-124, -120, 36, 39)
  
  for ( file in files ) {
    nc <- goesaodc_openFile(basename(file))
    bboxSubsetExample(nc, bbox, 2, TRUE)
  }
  
}


bboxSubsetExample <- function(
  nc = NULL,
  bbox = NULL,
  dqfLevel = 2,
  exponentiate = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(bbox)
  
  if ( is.null(exponentiate) || !is.logical(exponentiate) ) 
    exponentiate <- TRUE
  
  # ----- Prepare data ---------------------------------------------------------
  
  bbox <- bboxToVector(bbox)
  
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
  
  maps::map('state', xlim = c(bbox[1], bbox[2]), ylim = c(bbox[3], bbox[4]))

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
  
  maps::map('county', col = "white", add = TRUE)
  maps::map('state', lwd = 1.5, add = TRUE)
  
  # TODO:  Add title and other annotations
  
  
}

