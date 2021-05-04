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
#' @param dqfLevel Allowed data quality level. All readings with a DQF value
#' above this level will have their AOD values set to NA. Must be either 0, 1, 
#' 2, or 3, with 0 being the highest quality. Defaults to 3.

goesaodc_createScanPoints <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3
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
  
  if ( !(dqfLevel %in% c(0, 1, 2, 3)) )
    stop(paste0("Parameter 'dqfLevel' must be 0, 1, 2, or 3"))
  
  # ----- Create spatial points ------------------------------------------------
  
  result <- try({
    
    if ( is.null(filename) && !is.null(endtime) ) {
      
      # Average the AOD values across all scans in the time range
      spdf <- goesaodc_createMultiScanPoints(
        satID = satID,
        datetime = datetime,
        endtime = endtime,
        timezone = timezone,
        bbox = bbox,
        dqfLevel = dqfLevel
      )
      
    } else {
      
      # Get the AOD values for the scan closest to the requested time
      spdf <- goesaodc_createSingleScanPoints(
        satID = satID,
        datetime = datetime,
        timezone = timezone,
        filename = filename,
        bbox = bbox,
        dqfLevel = dqfLevel
      )
      
    }
    
  }, silent = TRUE)
  
  # If there was an error reading the scan file, create an SPDF filled with NA 
  # AOD and DQF values for the requested satellite
  if ( "try-error" %in% class(result) ) {
    spdf <- goesaodc_createEmptyScanPoints(satID, filename, bbox)
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
  
  bboxOregon <- c(-125, -116, 42, 47)
  
  # Create points for a scan specified by satellite + time
  goesaodc_createScanPoints(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = bboxOregon,
    dqfLevel = 3
  )
  
  # Create points for a scan file
  goesaodc_createScanPoints(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
    bbox = bboxOregon,
    dqfLevel = 2
  )
  
  # Create points from scans averaged over a time range
  goesaodc_createScanPoints(
    satID = "G17",
    datetime = "2020-09-08 12:00",
    endtime = "2020-09-08 13:00",
    timezone = "America/Los_Angeles",
    bbox = bboxOregon
  )
  
  # Create points for a faulty scan
  goesaodc_createScanPoints(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
    bbox = bboxOregon
  )
  
}