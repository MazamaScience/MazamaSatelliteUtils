#' @export
#'
#' @title List GOES AOD files available for a specific date and hour
#'
#' @description Retrieves a list of GOES AOD files for a specified date and hour
#' available either locally in the specified \code{setSatelliteDir} or on a 
#' remote server using the \code{useRemote} parameter.
#'
#' @details All files for a particular hour will be returned even if the
#' incoming \code{startdate} or \code{jdate} is specified to the minute or
#' second. By default, the local directory set by \code{setSatelliteDir()} is 
#' searched. \code{useRemote = TRUE} will search \code{baseURL} for all files 
#' that are available within the specified timeframe.
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param endtime End time in any Ymd H [MS] format or \code{POSIXct}
#' @param useRemote Logical specifying whether to look for files in 
#' \code{getSatelliteDataDir()} or \code{baseUrl}; Defaults to FALSE.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param isJulian Logical value determining whether \code{datetime} (and
#' optionally \code{endtime}) should be interpreted as a Julian date with day of
#'  year as a decimal number; Defaults to FALSE.
#' @param baseUrl URL of remote database; Defaults to 
#' "https://tools-1.airfire.org/Satellite/".
#'
#' @return Vector of filenames.
#'
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' # Local files for a single hour
#' goesaodc_listFiles("G17", "2019-09-06 18") 
#' 
#' # Remote files for a time range
#' goesaodc_listFiles(
#'   satID = "G16",
#'   datetime = "2019-09-06 06",
#'   endtime = "2019-09-06 18",
#'   timezone = "America/Los_Angeles",
#'   useRemote = TRUE
#' )
#' }

goesaodc_listFiles <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  useRemote = FALSE,
  timezone = "UTC",
  isJulian = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  
  # Convert satID to uniform case
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Parameter 'satID' must be either 'G16' (East) or 'G17' (West)")
  
  MazamaCoreUtils::stopIfNull(datetime)
  
  # Use timezone from POSIXct datetime, or passed in timezone
  if ( lubridate::is.POSIXt(datetime) ) {
    timezone <- lubridate::tz(datetime)
  } else {
    if ( !timezone %in% OlsonNames() ) {
      stop(sprintf("timezone \"%s\" is not recognized", timezone))
    }
  }
  
  if ( isJulian ) {
    
    # TODO: Julian day cannot exceed the number of days in the year
    
  } else {
    
    if ( !lubridate::is.POSIXt(datetime) ) {
      
      if ( is.na(lubridate::ymd_hms(datetime, tz = timezone, truncated = 2)) ) {
        stop("Parameter 'datetime' must be a POSIXt object or a string in either 'Ymd H' or Julian format")
      }
      
    } 
    
  }
  
  # Parse incoming times with MazamaCoreUtils
  datetime <- MazamaCoreUtils::parseDatetime(
    datetime, 
    timezone = timezone,
    isJulian = isJulian
  )
  
  if ( !is.null(endtime) ) {
    endtime <- MazamaCoreUtils::parseDatetime(
      endtime, 
      timezone = timezone, 
      isJulian = isJulian
    )
    
    if ( datetime > endtime )
      stop("Endtime is before datetime")
  }
  
  # ----- Prepare data ---------------------------------------------------------
  
  # Create satUrl for remote searching
  if ( useRemote ) {
    if ( satID == "G16" ) {
      satUrl <- paste0(baseUrl, "GOES-16/AODC")
    } else if ( satID == "G17" ) {
      satUrl <- paste0(baseUrl, "GOES-17/AODC")
    } else {
      stop("Parameter 'satID' must be either 'G16' or 'G17'")
    }
  }
  
  # Build nc file regex pattern
  ncFilePattern <- paste0(
    "OR_ABI-L2-AODC-M[0-9]_",
    satID,
    "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  )
  
  # ----- Create a list of files -----------------------------------------------
  
  if ( !useRemote ) {
    
    # Find all local nc files
    dataFiles <- list.files(getSatelliteDataDir(), pattern = ncFilePattern)
    
  } else {
    
    # Find all remote nc files
    links <-
      xml2::read_html(satUrl) %>%
      xml2::xml_child("body") %>%
      xml2::xml_child("table") %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")
    
    dataFiles <- links[ -(1:5) ]
    
  }
  
  # Build a list of requested hours
  if ( is.null(endtime) ) {
    
    hours <- c(datetime)
    
  } else {
    
    timeRange <- MazamaCoreUtils::timeRange(
      starttime = datetime,
      endtime = endtime,
      timezone = timezone
    )
    
    hours <- seq.POSIXt(from = timeRange[1], to = timeRange[2], by = "hour")
  
  }
  
  # Convert to UTC and create startPatterns for each requested hour
  startPatterns <- c(strftime(hours, "_s%Y%j%H", tz = "UTC"))
  
  # Find files that match startPatterns
  indicesList <- list()
  for ( startPattern in startPatterns ) {
    indicesList[[startPattern]] <- 
      which(stringr::str_detect(dataFiles, startPattern))
  }
  indices <- as.numeric(unlist(indicesList))
  
  # ----- Return ---------------------------------------------------------------
  
  matchingFiles <- dataFiles[indices]
  
  return(matchingFiles)
  
} # END OF FUNCTION

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  satID <- "G16"
  datetime <- 20190906# "2019-09-06 06:00"
  endtime <- NULL#"2019-09-06 18:00"
  timezone <- "America/Los_Angeles"
  useRemote <- FALSE
  baseUrl <- "https://tools-1.airfire.org/Satellite/"
  
  goesaodc_listFiles(
    satID = satID,
    datetime = datetime,
    endtime = endtime,
    timezone = timezone,
    useRemote = useRemote,
    baseUrl = baseUrl
  )
  
} 
