#' @export
#' 
#' @title List downloaded GOES AOD files for a specified date and hour
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param startdate desired date in any Ymd [H] format or \code{POSIXct}
#' @param jdate desired date in as a Julian date string, i.e. as seen in the
#'   netcdf filenames
#' @param fullDay Specifies that the user wants an entire day's worth of data
#' 
#' @description Retrieve a list of GOES AOD files available in the
#' \code{satelliteDataDir} for a specified date and hour.
#' 
#' Note that all files for a particular hour will be returned even if the
#' incoming \code{startdate} or \code{jdate} is specified to the minute or
#' second.
#' 
#' @return Vector of filenames.
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' date_with_hour <- "2019-09-06 16"
#' goesaodc_listFiles(satID = "G16", startdate = date_with_hour)
#' 
#' jdate <- "201924916"
#' goesaodc_listFiles(satID = "G17", jdate = jdate, fullDay = TRUE)
#' 
#' day_only <- "2019-09-06"
#' goesaodc_listFiles(satID = "G16", startdate = day_only)
#' }

goesaodc_listFiles <- function(
  satID = NULL,
  startdate = NULL,
  jdate = NULL,
  fullDay = FALSE
) {
  
  # VERIFY THAT satID HAS BEEN SPECIFIED
  if ( is.null(satID) ) {
    
    stop("GOES satID must be specified")
    
  } else {
    
    satID <- toupper(satID)
    
    if ( !(satID %in% c("G16", "G17")) ) {
      stop("Must specify GOES satellite ID (G16 or G17)")
      
    }
    
  }
  
  # IF A startdate HAS BEEN PASSED IN, ATTEMPT TO PARSE IT
  if ( !is.null(startdate) ) {
    
    suppressWarnings(
      starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC") )
    if (lubridate::hour(starttime) == 0) {
      fullDay <- TRUE
      
    }
    
    # OTHERWISE IF jdate PRESENT, CONVERT IT TO POSIXt
  } else if ( !is.null(jdate) ) {
    
    if (stringr::str_length(jdate) <= 7) {
      # ie 2019249
      fullDay <- TRUE
    } 
    
    # JDATE IS STRIPPED TO 13 CHARS AS 14TH WON'T PARSE (20192491646196)
    jdate <- stringr::str_sub(jdate, 1, 13)
    formats <- c("Yj", "YjH", "YjHMS")
    starttime <- lubridate::parse_date_time(jdate, orders = formats, tz = "UTC")
    
  } else {
    
    stop("Either 'startdate' or 'jdate' must be defined.", call. = FALSE)
    
  }
  
  # CONVERT starttime TO CORRECT JULIAN FORMAT BASED ON 'fullDay' PARAMETER
  if ( fullDay ) {
    
    startString <- strftime(starttime, "%Y%j", tz = "UTC")
    startString <- stringr::str_sub(startString, 1, 7)
    
  } else {
    
    startString <- strftime(starttime, "%Y%j%H", tz = "UTC")
    startString <- stringr::str_sub(startString, 1, 9)
    
  }
  
  # ASSEMBLE A LIST OF ALL .nc FILES IN SatelliteDataDir AND THEN LOOK IN THAT
  # LIST FOR THE PATTERN THAT MATCHES THE SPECIFIED starttime AND fullDay
  regex <- paste0("OR_ABI-L2-AODC-M[0-9]_",
                  satID,
                  "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc")
  
  dataFiles <- list.files(getSatelliteDataDir(), pattern = regex)
  
  startStrings <- purrr::map_chr(dataFiles, goesaodc_getStartString)
  
  # Find matching start times
  mask <- stringr::str_detect(startStrings, startString)
  matchingFiles <- dataFiles[mask]
  
  return(matchingFiles)
  
} # END OF FUNCTION
