#' @title Create spatial points for a single scan
#' 
#' @description Creates a SpatialPointsDataFrame from a single scan file 
#' specified either by a filename or through satellite and time information.
#' 
#' @details If \code{filename} is given, \code{satID}, \code{datetime}, and
#' \code{timezone} will be ignored.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param filename The name of the scan file.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level; Defaults to NULL.

goesaodc_createSingleScanSPDF <- function(
  satID = NULL,
  datetime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(filename) ) {
    
    MazamaCoreUtils::stopIfNull(satID)
    
    satID <- toupper(satID)
    if ( !(satID %in% c("G16", "G17")) )
      stop("Parameter 'satID' must be either 'G16' or 'G17'")
    
    MazamaCoreUtils::stopIfNull(datetime)
    
    # Use timezone from POSIXct datetime, or passed in timezone
    if ( lubridate::is.POSIXt(datetime) ) {
      timezone <- lubridate::tz(datetime)
    } else {
      if ( !timezone %in% OlsonNames() ) {
        stop(sprintf("timezone \"%s\" is not recognized", timezone))
      }
    }
    
  }
  
  # ----- Get scan file --------------------------------------------------------
  
  if ( is.null(filename) ) {
    
    # Find which scan file is closest to the requested time
    filename <- goasaodc_findClosestScanFile(
      satID = satID,
      datetime = datetime,
      timezone = timezone
    )
    
  }
  
  # Download the file if it isn't available locally
  if ( !(filename %in% list.files(getSatelliteDataDir())) ) {
    
    satUrl <- ifelse(satID == "G16", "GOES-16/AODC/", "GOES-17/AODC/")
    
    fileUrl <- paste0(
      "https://tools-1.airfire.org/Satellite/",
      satUrl,
      filename
    )
    
    filePath <- file.path(getSatelliteDataDir(), filename)
    
    utils::download.file(
      fileUrl,
      destfile = filePath, 
      quiet = TRUE, 
      method = "auto", 
      mode = "wb"
    )
    
  }
  
  # ----- Create spatial points ------------------------------------------------
  
  nc <- goesaodc_openFile(filename)
  
  tbl <- goesaodc_createTibble(nc, bbox, dqfLevel)
  
  spdf <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
    data = dplyr::select(tbl, -c(.data$lon, .data$lat))
  )
    
  # ----- Return ---------------------------------------------------------------
  
  return(spdf)
  
}

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  
  loadSpatialData("NaturalEarthAdm1")
  
  bbox_oregon <- c(-125, -116, 42, 47)
  
  # Create points from a scan specified by satellite and time
  goesaodc_createSingleScanSPDF(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    dqfLevel = 3
  )
  
  filename <- "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc"
  
  title <- 
    filename %>%
    goesaodc_convertFilenameToDatetime() %>%                         # UTC
    MazamaCoreUtils::parseDatetime(timezone = "America/Los_Angeles") # Local
  
  # Create points from a named scan file
  spdf <- goesaodc_createSingleScanSPDF(
    filename = filename,
    bbox = bbox_oregon,
    dqfLevel = 3
  )
  
  # Plot points
  goesaodc_plotScanSPDF(spdf, bbox = bbox_oregon, title = title) +
    AirFirePlots::layer_states("OR")
  
}

