#' @export
#' 
#' @importFrom graphics par
#' 
#' @title Create plots of AOD data available within a region
#' 
#' @description The goal of this plot is to get a quick look at available data 
#' within a region. A user might look at a single timestep or might pass in a 
#' list of nc handles to see if native grid averaging over several time steps 
#' significantly reduces the number missing data grid cells.
#' 
#' @param nc ncdf4 handle or a list of handles.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level; Defaults to 2.
#' @param col_state Color of state borders. Use "transparent" for no lines.
#' @param col_county Color of county borders. Use "transparent" for no lines.
#' @param lwd_state Line weight of state borders.
#' @param lwd_county Line weight of county borders.
#' @param ... Additional arguments passed to \code{goesaodc_plotSpatialPoints()}.
#' 
#' @examples
#' \dontrun{
#' library(sp)
#' library(MazamaSpatialUtils)
#' library(MazamaSatelliteUtils)
#' 
#' setSpatialDataDir("~/Data/Spatial")
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' loadSpatialData("USCensusStates")
#' 
#' # Date and region of interest
#' files <- goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = 2019102714, 
#'   timezone = "America/Los_Angeles"
#' )
#'   
#' # Kincade fire region
#' kincade_bbox <- c(-124, -120, 36, 39)
#' 
#' # Plot a single NetCDF file
#' netcdf <- goesaodc_openFile(basename(files[1]))
#' goesaodc_areaPlot(netcdf, kincade_bbox)
#' 
#' # Plot multiple NetCDF files
#' netcdfs <- list()
#' for ( file in files ) {
#'   label <- 
#'     file %>%
#'     goesaodc_convertFilenameToDatetime() %>%
#'     MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
#'   netcdfs[[label]] <- goesaodc_openFile(basename(file))
#' }
#' goesaodc_areaPlot(netcdfs, kincade_bbox)
#' }

goesaodc_areaPlot <- function(
  nc = NULL,
  bbox = bbox_CONUS,        
  dqfLevel = 2,
  col_state = "black",
  col_county = "white",
  lwd_state = 1.5,
  lwd_county = 1,          
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(bbox)
  
  # Force a single ncdf4 handle into a list
  if ( "list" %in% class(nc) ) {
    ncList <- nc
  } else if ( "ncdf4" %in% class(nc) ) {
    ncList <- list(nc)
  } else {
    stop("Parameter 'nc' must be of type 'list' or 'ncdf4'")
  }
  
  # ----- Arguments for goesaodc_plotSpatialPoints() ---------------------------
  
  argsList <- list(
    var = "AOD",
    ... = ...,
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
    argsList$cex <- 0.75
  
  # ---- Process and plot each .nc handle --------------------------------------
  
  for (netCdf in ncList) {
    
    # ----- Prepare data -------------------------------------------------------
    
    bbox <- bboxToVector(bbox)
    xlim <- c(bbox[1], bbox[2])
    ylim <- c(bbox[3], bbox[4])
    
    # Load data from within bbox and create SpatialPoints
    result <- try({
      SP <- goesaodc_createSpatialPoints(netCdf, bbox, dqfLevel)
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
    
    # ----- Create plot --------------------------------------------------------
    
    maps::map("state", xlim = xlim, ylim = ylim)
    
    usr <- par("usr")
    graphics::rect(usr[1], usr[3], usr[2], usr[4], col = "gray80")
    
    if ( noData ) {
      
      x <- (usr[2] - usr[1]) / 2
      
      graphics::text(
        x = usr[1] + (usr[2] - usr[1]) / 2,
        y = usr[3] + (usr[4] - usr[3]) / 2,
        sprintf("No Data Found for dqfLevel %d", dqfLevel)
      )
      
    } else {
      
      argsList$spatialPoints <- SP
      do.call(goesaodc_plotSpatialPoints, argsList)

    }
    
    maps::map(
      'county',
      xlim = xlim,
      ylim = ylim,
      col = col_county,
      lwd = lwd_county,
      add = TRUE
    )
    maps::map(
      'state',
      xlim = xlim,
      ylim = ylim,
      col = col_state,
      lwd = lwd_state,
      add = TRUE
    )
    
    # TODO:  Add title and other annotations
    
  } # END OF NC HANDLE FOR LOOP
  
} # END OF FUNCTION


