#' @export
#' 
#' @title List downloaded GOES AOD files for a specified date and hour
#' 
#' @param date desired date (integer, characer representing YYYYMMDD)
#' @param hour desired hour (integer, character representing HH)
#' 
#' @description Retrieve a list of GOES AOD files available in the
#' satelliteDataDir for a specified date and hour
#' 
#' @return Vector of matching files

goesaodc_listFiles <- function(
  date,
  hour = NULL
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  date <- as.character(date)
  if (stringr::str_length(date) != 8) {
    stop(paste0("'date' must be of the format YYYYMMDD"))
  }
  
  if (!is.null(hour)) {
    hour <- as.character(hour)
    if (stringr::str_length(hour) > 2) {
      stop(paste0("'hour' must be of the format HH"))
    }
  }
  
  # ----- Get Matching Files ---------------------------------------------------
  
  dataDir <- getSatelliteDataDir()
  
  regex <- "OR_ABI-L2-AODC-M6_G16_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  dataFiles <- list.files(getSatelliteDataDir(), pattern = regex)
  startTimes <- purrr::map_chr(dataFiles, goesaodc_getStartTime)
  
  # Convert date and hour to POSIXct, then to Julian date
  if ( is.null(hour) ) {
    startTime <- lubridate::parse_date_time(date, orders = "%Y%m%d")
    startTime <- format(startTime, "%Y%j")
  } else {
    startTime <- lubridate::parse_date_time(paste0(date, hour), 
                                            orders = "%Y%m%d%H")
    startTime <- format(startTime, "%Y%j%H")
  }
  
  # Find matching start times
  mask <- stringr::str_detect(startTimes, startTime)
  matchingFiles <- dataFiles[mask]
  
  return(matchingFiles)
}

