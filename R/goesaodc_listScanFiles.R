#' @export
#'
#' @title List GOES AOD scan files
#'
#' @description Lists GOES AOD scan files for a given datetime or time range. If
#' just \code{datetime} is given, then only the scan file closest to that time
#' will be listed. If \code{endtime} is specified as well, then all scans from
#' \code{datetime} up to (but not including) \code{endtime} will be listed.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:30",
#'   timezone = "UTC",
#'   useRemote = TRUE
#' )
#' 
#' goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "UTC",
#'   useRemote = TRUE
#' )
#' }

goesaodc_listScanFiles <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  isJulian = FALSE,
  useRemote = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  
  # Convert satID to uniform case
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Parameter 'satID' must be either 'G16' (East) or 'G17' (West)")
  
  MazamaCoreUtils::stopIfNull(datetime)
  
  # Use timezone from POSIXt datetime or the given timezone
  if ( lubridate::is.POSIXt(datetime) ) {
    timezone <- lubridate::tz(datetime)
  } else {
    if ( !timezone %in% OlsonNames() )
      stop(sprintf("timezone \"%s\" is not recognized", timezone))
  }
  
  # ----- Get all scan files ---------------------------------------------------
  
  # Create satUrl for remote searching
  if ( useRemote ) {
    if ( satID == "G16" ) {
      satUrl <- paste0(baseUrl, "GOES-16/AODC")
    } else if ( satID == "G17" ) {
      satUrl <- paste0(baseUrl, "GOES-17/AODC")
    } else {
      stop("Parameter 'satID' must be either 'G16' (East) or 'G17' (West)")
    }
  }
  
  # Build pattern for scan files
  scanFilePattern <- paste0("OR_ABI-L2-AODC-M[0-9]_", satID, "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc")
  
  if ( !useRemote ) {
    
    # Create a list of all local scan files
    allScanFiles <- list.files(getSatelliteDataDir(), pattern = scanFilePattern)
    
  } else {
    
    # Create a list of all remote scan files
    links <-
      xml2::read_html(satUrl) %>%
      xml2::xml_child("body") %>%
      xml2::xml_child("table") %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")
    
    allScanFiles <- links[ -(1:5) ]
    
  }
  
  # ----- Find scan files for the requested time -------------------------------
  
  # Parse datetime as POSIXt
  datetime <- MazamaCoreUtils::parseDatetime(
    datetime, 
    timezone = timezone,
    isJulian = isJulian
  )
  
  # Find the scan file closest to datetime
  startFilename <- goesaodc_findClosestScanFile(
    scanFiles = allScanFiles,
    datetime = datetime
  )
  
  startClosestIndex <- which(startFilename == allScanFiles)[1]
  
  # Return all the scan files in the time range
  if ( !is.null(endtime) ) {
    
    # Parse endtime as POSIXt
    endtime <- MazamaCoreUtils::parseDatetime(
      endtime, 
      timezone = timezone, 
      isJulian = isJulian,
    )
    
    # Find the scan file closest to endtime
    endFilename <- goesaodc_findClosestScanFile(
      scanFiles = allScanFiles,
      datetime = endtime
    )
    
    endClosestIndex <- which(endFilename == allScanFiles)[1]
    
    # The first scan file must be >= datetime
    startScanTimeDiff <- goesaodc_convertFilenameToDatetime(startFilename) - datetime
    startIndex <- ifelse(startScanTimeDiff < 0, startClosestIndex + 1, startClosestIndex)
      
    # The last scan file must be < endtime
    endScanTimeDiff <- goesaodc_convertFilenameToDatetime(startFilename) - datetime
    endIndex <- ifelse(endScanTimeDiff >= 0, endClosestIndex - 1, endClosestIndex)
    
    scanFiles <- allScanFiles[startIndex:endIndex]
    
    return(scanFiles)
    
  }
  
  # Return the scan file closest to datetime
  scanFile <- allScanFiles[startClosestIndex]
  
  # ----- Return ---------------------------------------------------------------
  
  return(scanFile)
  
}


goesaodc_findClosestScanFile <- function(
  scanFiles = NULL,
  datetime = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !("character" %in% class(scanFiles)) )
    stop("Parameter 'scanFiles' must be a vector of strings")
  
  # ----- Subset scan files ----------------------------------------------------
  
  # Find files covering the hour of the requested time
  # TODO: It's possible that the closest scan is in the next/prev hour, or day, 
  # or year...
  hourScanFilesPattern <- strftime(datetime, "_s%Y%j%H", tz = "UTC")
  hourScanFilesIndices <- which(stringr::str_detect(scanFiles, hourScanFilesPattern))
  hourScanFiles <- scanFiles[hourScanFilesIndices]
  
  if ( length(hourScanFiles) == 0 )
    stop("No scan files found for this hour.")
  
  # ----- Find closest scan file -----------------------------------------------
  
  closestScanFile <- hourScanFiles[1]
  shortestDuration <- lubridate::time_length(lubridate::duration(1, "hours"))
  
  for ( scanFile in hourScanFiles ) {
    
    # Convert filename to POSIXt
    scanFileDatetime <- goesaodc_convertFilenameToDatetime(scanFile)
    
    # Calculate difference from requested datetime
    interval <- lubridate::interval(scanFileDatetime, datetime)
    duration <- lubridate::time_length(interval)
    
    # Check if this is the closest scan to the requested datetime
    if ( abs(duration) < shortestDuration ) {
      closestScanFile = scanFile
      shortestDuration <- duration
    }
    
    # TODO: Quick exit if current interval is longer than the last. Probably 
    # overkill if an hour only has ~12 scans
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(closestScanFile)
  
}
