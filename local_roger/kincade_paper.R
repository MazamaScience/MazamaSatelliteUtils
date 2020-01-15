# Kincade Fire Plot for 10/27/2019
# UTC Time = "2019-10-27 22:16:00 UTC"

#  Taken from goesaodc_areaPLot()
library(sp)
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")

library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

# Date and region of interest
files <- goesaodc_downloadAOD(satID = "G17",
  datetime = 2019102715,
  timezone = "America/Los_Angeles")[4]

#' Kincade fire
bbox <- c(-126, -118, 34.75, 40.25)

# Build a list of open nc handles to process
ncList <- list()
for ( file in files ) {
  label <-
    file %>%
    goesaodc_convertFilenameToDatetime() %>%
    MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
  ncList[[label]] <- goesaodc_openFile(basename(file))
}

# Some additional params
dqfLevel = 3
col_state = "black"
col_county = "white"
lwd_state = 1.5
lwd_county = 1
  
  # ----- Arguments to goesaodc_plotSpatialPoints() ----------------------------
  
  argsList <- list(
    var = "AOD",
    add = TRUE
  )
  
  # Sample size
  if ( !("n" %in% names(argsList)) )
    argsList$n <- 5e5
  
  # Breaks
  if ( !("breaks" %in% names(argsList)) )
    argsList$breaks <- c(-Inf, 0.2, 0.5, 1, 2, Inf)
  
  # Number of color bins
  if ( !("colBins" %in% names(argsList)) )
    argsList$colBins <- 5
  
  # Palette
  if ( !("paletteName" %in% names(argsList)) )
    argsList$paletteName <- "YlOrRd"
  
  # Symbol
  if ( !("pch" %in% names(argsList)) )
    argsList$pch <- 15
  
  # Symbol size
  if ( !("cex" %in% names(argsList)) )
    argsList$cex <- 0.35
  
  # ---- Process and display each .nc handle -----------------------------------
  
  for (nc in ncList) {
    
    # ----- Prepare data -------------------------------------------------------
    bbox <- bboxToVector(bbox)
    xlim <- c(bbox[1], bbox[2])
    ylim <- c(bbox[3], bbox[4])
    
    # Load data from within bbox and create SpatialPoints
    SP <- goesaodc_createSpatialPoints(nc, bbox, dqfLevel)
    
    # ----- Create plot --------------------------------------------------------
    maps::map('state', xlim = xlim, ylim = ylim)
    
    # Plot a gray background
    usr <- par('usr')
    graphics::rect(usr[1], usr[3], usr[2], usr[4], col = "gray80")
    
    # Plot the spatial points
    argsList$spatialPoints <- SP
    do.call(goesaodc_plotSpatialPoints, argsList)
    
    # Plot the county and state lines
    maps::map('county', xlim = xlim, ylim = ylim,
              col = col_county, lwd = lwd_county, add = TRUE)
    maps::map('state', xlim = xlim, ylim = ylim,
              col = col_state, lwd = lwd_state, add = TRUE)
    
    # TODO:  Add title and other annotations
    
  } # END OF NC HANDLE FOR LOOP


