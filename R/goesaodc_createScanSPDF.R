#' @export
#' 
#' @title Create spatial points from GOES scans
#' 
#' @description Creates a SpatialPointsDataFrame of AOD readings from one or 
#' more GOES scans. Scans are specified either by filename or satellite + time 
#' information.
#' 
#' @details
#' If \code{filename} is given, just points for that scan file will be 
#' processed. Otherwise, scan(s) must be specified by \code{satID}, 
#' \code{datetime}, and \code{timezone}, which will determine the closest scan 
#' taken to that time. An average SPDF of a series of scans can be requested by 
#' also supplying \code{endtime}. All scans taken from \code{datetime} 
#' (inclusive) up to \code{endtime} (exclusive) will be processed by taking the 
#' average value of each point.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param endtime End time in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param filename Name of a scan file.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level; Defaults to NULL.

goesaodc_createScanSPDF <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
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
    
    # Use timezone from POSIXct datetime, or the given timezone
    if ( lubridate::is.POSIXt(datetime) ) {
      timezone <- lubridate::tz(datetime)
    } else {
      MazamaCoreUtils::stopIfNull(timezone)
      if ( !timezone %in% OlsonNames() )
        stop(sprintf("timezone \"%s\" is not recognized", timezone))
    }
    
  }
  
  # ----- Create spatial points ------------------------------------------------
  
  result <- try({
    
    if ( is.null(filename) && !is.null(endtime) ) {
      
      # Average the AOD values across all scans in the time range
      spdf <- goesaodc_createMultiScanSPDF(
        satID = satID,
        datetime = datetime,
        endtime = endtime,
        timezone = timezone,
        bbox = bbox,
        dqfLevel = dqfLevel
      )
      
    } else {
      
      # Get the AOD values for the scan closest to the requested time
      spdf <- goesaodc_createSingleScanSPDF(
        satID = satID,
        datetime = datetime,
        timezone = timezone,
        filename = filename,
        bbox = bbox,
        dqfLevel = dqfLevel
      )
      
    }
    
  }, silent = TRUE)
  
  # Create an empty SPDF if there was an error reading the scan file
  if ( "try-error" %in% class(result) ) {
    
    spdf <- sp::SpatialPointsDataFrame(
      coords = data.frame(lon = 0, lat = 0),
      data = data.frame(AOD = 0)
    )[-1,]
    
    warning(result, immediate. = TRUE)
    
  }
  
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
  
  # Create points from a scan specified by satellite + time
  goesaodc_createScanSPDF(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    dqfLevel = 3
  )
  
  # Create points from a named scan file
  goesaodc_createScanSPDF(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
    bbox = bbox_oregon,
    dqfLevel = 3
  )
  
  # Create averaged points from scans covering a time range
  spdf <- goesaodc_createScanSPDF(
    satID = "G17",
    datetime = "2020-09-08 12",
    endtime = "2020-09-08 13",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon
  )
  
  # Plot averaged points
  goesaodc_plotScanSPDF(spdf, bbox = bbox_oregon) +
    AirFirePlots::layer_states("OR")
  
}