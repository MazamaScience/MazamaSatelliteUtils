# 4/30/2019

# ----- goesaodc_open Draft ----------------------------------------------------

# @export
# 
# @title Open a GOES AOD netCDF file as a ncdf4 object
# @param startTime Scan start time (integer, character representing YYYY[MMDDMMSSS])
# 
# @description If \code{startTime} matches a single GOES AOD netCDF file in 
# the \code{satelliteDataDir}, then return a \code{ncdf4} object for that file.
# If multiple matches are found, print a list of matching start times.

goesaodc_open <- function(
  startTime
) {
  
  # ----- Validate Paramters ---------------------------------------------------
  
  if(!lubridate::is.POSIXct(startTime)) {  # skip rest of validation if startTime is POSIXct
    startTime <- as.character(startTime)
    if ( stringr::str_length(startTime) == 4 ) { # YYYY
      order <- "%Y"
      startTime <- lubridate::parse_date_time(startTime, order)
    }
    if ( stringr::str_length(startTime) == 6 ) { # YYYYMM
      startTime <- lubridate::parse_date_time(startTime, "Ym")
    }  
    if ( stringr::str_length(startTime) == 8 ) { # YYYYMMDD
      startTime <- lubridate::parse_date_time(startTime, "YmD")
    }  
    if ( stringr::str_length(startTime) == 10 ) { # YYYYMMDDHH
      startTime <- lubridate::parse_date_time(startTime, "YmDH")
    }
    if ( stringr::str_length(startTime) == 12 ) { # YYYYMMDDHHMM
      startTime <- lubridate::parse_date_time(startTime, "YmDHM")
    }
    if ( stringr::str_length(startTime) == 14 ) { # YYYYMMDDHHMMSS
      startTime <- lubridate::parse_date_time(startTime, "YmDHS")
    }  
  }
  
  
  
    
  ) {
    stop(paste0("'startTime' must be of the format YYYY[MMDDMMSSS]"))
  }
  
  # ----- Gather Matching Files ------------------------------------------------
  
  dataDir <- getSatelliteDataDir()
  
  regex <- "OR_ABI-L2-AODC-M6_G16_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  dataFiles <- list.files(getSatelliteDataDir(), pattern = regex)
  startTimes <- purrr::map_chr(dataFiles, goesaodc_getStartTime)
  
  # First, convert startTime to POSIXct
  startTime <- lubridate::as_datetime(startTime)
  
  # Then, convert startTime to Julian date
  if ( stringr::str_length(startTime) == 4 ) {
    startTime <- format(startTime, "%Y")
  } 
  if ( stringr::str_length(startTime) == 4 ) {
    startTime <- format(startTime, "%Y%j")
  } 
  
  matchingStartTimes <- stringr::str_subset(startTimes, startTime)
  
  print(matchingStartTimes)
  # return ncdf4 handle or print list of matches
}

