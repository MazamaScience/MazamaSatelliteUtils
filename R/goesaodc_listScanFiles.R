#' @export
#'
#' @title List GOES AOD scan files
#'
#' @description Lists GOES AOD scan files for a given datetime or time range. If
#' just \code{datetime} is given, then only the scan file closest to that time
#' will be listed. If \code{endtime} is specified as well, then all scans from
#' \code{datetime} up to (but not including) \code{endtime} will be listed.
#' 
#' @details If \code{useRemote=FALSE} and there are no scan files available for 
#' the day of the requested \code{datetime}, then \code{NULL} will be returned.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime as a Ymd HMS or Julian formatted string, or a 
#' \code{POSIXct}.
#' @param endtime End time as a Ymd HMS or Julian formatted string, or a 
#' \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}. Defaults to UTC.
#' @param isJulian Logical value determining whether \code{datetime} (and
#' optionally \code{endtime}) should be interpreted as a Julian date with day of
#'  year as a decimal number. Defaults to FALSE.
#' @param useRemote Logical specifying whether to look for files in 
#' \code{getSatelliteDataDir()} or \code{baseUrl}. Defaults to FALSE.
#' @param baseUrl URL of remote database. Defaults to 
#' "https://tools-1.airfire.org/Satellite/".
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
  
  # ----- Get all available scan files -----------------------------------------
  
  if ( useRemote ) {
    
    # Create url for remote searching
    if ( useRemote ) {
      if ( satID == "G16" ) {
        satUrl <- paste0(baseUrl, "GOES-16/AODC")
      } else if ( satID == "G17" ) {
        satUrl <- paste0(baseUrl, "GOES-17/AODC")
      } else {
        stop("Parameter 'satID' must be either 'G16' (East) or 'G17' (West)")
      }
    }
    
    # Create a list of all remote scan files
    links <-
      xml2::read_html(satUrl) %>%
      xml2::xml_child("body") %>%
      xml2::xml_child("table") %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")
    
    allScanFiles <- links[ -(1:5) ]
    
  } else {
    
    # Create a list of all local scan files
    scanFilePattern <- paste0("OR_ABI-L2-AODC-M[0-9]_", satID, "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc")
    allScanFiles <- list.files(getSatelliteDataDir(), pattern = scanFilePattern)
    
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
  
  if ( is.null(startFilename) ) {
    warning(
      "No scan files found in the same day as the requested 'datetime'. If
      parameter 'useRemote=FALSE', try running 'goesaodc_downloadScanFiles()' 
      first.",
      immediate. = TRUE
    )
    return(NULL)
  }
  
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
    
    if ( is.null(endFilename) ) {
      warning(
        "No scan files found in the same day as the requested 'endtime'. If 
        parameter 'useRemote=FALSE', try running 'goesaodc_downloadScanFiles()' 
        first.",
        immediate. = TRUE
      )
      return(NULL)
    }
    
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
  
  # Find files covering the day of the requested time
  # TODO: It's possible that the closest scan is in the next or previous day, or
  # year...
  
  dayFilesPattern <- strftime(datetime, "_s%Y%j", tz = "UTC")
  dayFilesIndices <- which(stringr::str_detect(scanFiles, dayFilesPattern))
  dayFilenames <- scanFiles[dayFilesIndices]
  
  if ( length(dayFilenames) == 0 ) {
    warning("No scan files found for the requested day.", immediate. = TRUE)
    return(NULL)
  }
  
  # ----- Find closest scan file -----------------------------------------------
  
  closestFilename <- dayFilenames[1]
  shortestDuration <- lubridate::time_length(lubridate::duration(1, "year"))
  
  for ( filename in dayFilenames ) {
    
    # Convert filename to POSIXt
    fileDatetime <- goesaodc_convertFilenameToDatetime(filename)
    
    # Calculate difference from requested datetime
    interval <- lubridate::interval(fileDatetime, datetime)
    duration <- lubridate::time_length(interval)
    
    # Check if this is the closest scan to the requested datetime
    if ( abs(duration) < shortestDuration ) {
      closestFilename <- filename
      shortestDuration <- duration
    }
    
    # TODO: Quick exit if the current duration is longer than the last
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(closestFilename)
  
}
