#' @export
#' 
#' @title Create plots of AOD data available within a region
#' 
#' @param ncList ncdf4 handle or a list of handles.
#' @param bbox Geographic extent of area of interest; Defaults to CONUS.
#' @param dqfLevel Sets the DQF level to filter to data to.
#' @param exponentiate Logical, sets whether to scale the data values by 10^AOD 
#' @param col_state Color of state borders. Use "transparent" for no lines.
#' @param col_county Color of county borders. Use "transparent" for no lines.
#' @param lwd_state Line weight of state borders.
#' @param lwd_county Line weight of county borders.
#' 
#' @description The goal of this plot is to get a quick look at available data 
#' within a region. A user might look at a single timestep or might pass in a 
#' list of nc handles to see if native grid averaging over several time steps 
#' significantly reduces the number missing data grid cells.
#' 
#' @examples
#' \dontrun{
#' library(sp)
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusStates")
#' 
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' # Date and region of interest
#' files <- goesaodc_downloadAOD(satID = "G17", 
#'   datetime = 2019102714, 
#'   timezone = "America/Los_Angeles")[1]
#'   
#' #' Kincade fire
#' bbox <- c(-124, -120, 36, 39)
#' 
#' # Build a list of open nc handles to process
#' ncList <- list()
#' for ( file in files ) {
#'   label <- 
#'     file %>%
#'     goesaodc_convertFilenameToDatetime() %>%
#'     MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
#'   ncList[[label]] <- goesaodc_openFile(basename(file))
#' }
#' 
#' goesaodc_areaPlot(ncList, kincade_bbox)
#' }


goesaodc_areaPlot <- function(
  ncList = NULL,  # a single or list of nc handles
  box = bbox_CONUS,        
  dqfLevel = 2,
  exponentiate = TRUE,
  col_state = "black",
  col_county = "white",
  lwd_state = 1.5,
  lwd_county = 1             
  # additional arguments passed to goesaodc_plotSpatialPoints()
) {
  
  # ----- Validate parameters --------------------------------------------------
  MazamaCoreUtils::stopIfNull(ncList)
  MazamaCoreUtils::stopIfNull(bbox)
  
  # ---- Process and display each .nc handle -----------------------------------
  for (nc in ncList) {
    
    # Check if we should exponentiate
    if ( is.null(exponentiate) || !is.logical(exponentiate) ) 
      exponentiate <- TRUE
    
    # ----- Prepare data ---------------------------------------------------------
    bbox <- bboxToVector(bbox)
    
    # Load data from within bbox and create SpatialPoints
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
    
    # Exponentiate the data
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
      # No breaks if not exponentiated
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
    
    maps::map('county', col = col_county, lwd = lwd_county, add = TRUE)
    maps::map('state', col = col_state, lwd = lwd_state, add = TRUE)
    
    # TODO:  Add title and other annotations
  } # END OF NC HANDLE FOR LOOP
  
} # END OF FUNCTION


