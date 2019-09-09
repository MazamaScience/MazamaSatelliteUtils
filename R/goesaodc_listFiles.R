#' @export
#' 
#' @title List downloaded GOES AOD files for a specified date and hour
#' 
#' @param startdate desired date in any Y-m-d [H] format or \code{POSIXct}
#' @param jdate desired date in as a Julian date string, i.e. as seen in the
#'   netcdf filenames
#' @param satId ID number of the source GOES satellite
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
#' \dontrun{
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' date <- lubridate::ymd_h("2019-05-16 16", tz = "UTC")
#' files <- goesaodc_listFiles(date, satId = 16)
#' print(files)
#' }

goesaodc_listFiles <- function(
  startdate = NULL,
  jdate = NULL,
  satId = NULL
) {
  
  # ----- Parse incoming date --------------------------------------------------
  
  if ( !is.null(startdate) ) {
    
    # Is it a full day?
    suppressWarnings({
      starttime <- lubridate::parse_date_time(startdate, "Ymd", tz = "UTC") 
    })
    if ( !is.na(starttime) ) {
      fullDay <- TRUE
    } else {
      orders <- c("YmdH","YmdHM","YmdHMS")
      suppressWarnings({
        starttime <- lubridate::parse_date_time(startdate, orders, tz = "UTC")
      })
      if ( is.na(starttime) ) {
        stop("Parameter 'startdate' cannot be interpreted. Is it a 'jdate'?")
      }
      fullDay <- FALSE
    }
    
  } else if ( !is.null(jdate) ) {
    
    # Check for operator error
    if ( !is.numeric(jdate) && !is.character(jdate) ) {
      jdate_class <- paste(class(jdate), sep = ", ")
      stop(paste0("Parameter 'jdate' cannot be of class '", jdate_class, "'"), call. = FALSE)
    }
    
    jdate <- as.character(jdate)
    
    # Check for operator error
    if ( stringr::str_detect(jdate, "[^0-9]") ) {
      stop(paste0("'", jdate, "' is not a Julian date string."), call. = FALSE)
    }
    
    if ( stringr::str_count(jdate) == 5 ) {
      starttime <- strptime(jdate, "%Y%j", tz = "UTC")
      fullDay <- TRUE
    } else if ( stringr::str_count(jdate) == 7 ) {
      starttime <- strptime(jdate, "%Y%j%H", tz = "UTC")
      fullDay <- FALSE
    } else if ( stringr::str_count(jdate) == 9 ) {
      starttime <- strptime(jdate, "%Y%j%H", tz = "UTC")
      fullDay <- FALSE
    } else {
      # strip the string down to the YjH level
      jdate <- stringr::str_sub(jdate, end = 9)
      starttime <- strptime(jdate, "%Y%j%H", tz = "UTC")
      fullDay <- FALSE
    }
    
  } else {
    
    stop("Either 'startdate' or 'jdate' must be defined.", call. = FALSE)
    
  }
  
  if (is.null(satId)) {
    stop("Must specify GOES satellite ID (16 or 17)")
  }
  
  # Julian string for comparison with file names
  if ( fullDay ) {
    startString <- strftime(starttime, "%Y%j", tz = "UTC")
  } else {
    startString <- strftime(starttime, "%Y%j%H", tz = "UTC")
  }

  # ----- Get Matching Files ---------------------------------------------------
  
  # regex <- "OR_ABI-L2-AODC-M[0-9]_G16_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  regex <- paste0("OR_ABI-L2-AODC-M[0-9]_G", satId, "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc")
  dataFiles <- list.files(getSatelliteDataDir(), pattern = regex)
  startStrings <- purrr::map_chr(dataFiles, goesaodc_getStartString)
  
  # Find matching start times
  mask <- stringr::str_detect(startStrings, startString)
  matchingFiles <- dataFiles[mask]
  
  return(matchingFiles)
  
}
