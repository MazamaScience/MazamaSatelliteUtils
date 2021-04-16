#' @export
#' 
#' @title Find the scan file closest to a datetime
#' 
#' @description Finds the name of the scan file that was generated closest to 
#' the given datetime. 
#' 
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Datetime in any Ymd HMS [MS] format or \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime}.
#' @param baseUrl URL of remote database; Defaults to 
#' "https://tools-1.airfire.org/Satellite/".
#' 
#' @return A scan filename.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' closestScanFile <- goasaodc_findClosestScanFile(
#'   satID = "G17",
#'   datetime = "2020-09-08 08:30:00",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' print(MazamaCoreUtils::parseDatetime(
#'   goesaodc_convertFilenameToDatetime(closestScanFile),
#'   "America/Los_Angeles"
#' ))
#' }

goasaodc_findClosestScanFile <- function(
  satID = NULL,
  datetime = NULL,
  timezone = NULL,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(timezone)
  
  # ----- Gather candidate scan files  -----------------------------------------
  
  if ( satID == "G16" ) {
    satUrl <- paste0(baseUrl, "GOES-16/AODC")
  } else if ( satID == "G17" ) {
    satUrl <- paste0(baseUrl, "GOES-17/AODC")
  } else {
    stop("Parameter 'satID' must be either 'G16' or 'G17'")
  }
  
  # Build nc file regex pattern
  ncFilePattern <- paste0(
    "OR_ABI-L2-AODC-M[0-9]_",
    satID,
    "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  )
  
  # Find all remote nc files
  links <-
    xml2::read_html(satUrl) %>%
    xml2::xml_child("body") %>%
    xml2::xml_child("table") %>%
    xml2::xml_find_all("//a") %>%
    xml2::xml_attr("href")
  
  scanFiles <- links[ -(1:5) ]
  
  datetime <- MazamaCoreUtils::parseDatetime(
    datetime = datetime,
    timezone = timezone
  )
  
  # Define a pattern for filenames covering the requested hour
  startPattern <- strftime(datetime, "_s%Y%j%H", tz = "UTC")
  
  # Find files that match startPatterns
  indices <- which(stringr::str_detect(scanFiles, startPattern))
  matchingScanFiles <- scanFiles[indices]
  
  if ( length(matchingScanFiles) == 0 )
    stop("No scan files found for this hour.")
  
  # ----- Find closest scan file -----------------------------------------------
  
  # Find which scan file is closest to the requested datetime
  closestScanFile <- matchingScanFiles[1]
  shortestDuration <- lubridate::time_length(lubridate::duration(1, "hours"))
  
  for ( scanFile in matchingScanFiles ) {
    
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
