#' @export
#'
#' @title List available GOES AOD files for a specified date and hour
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Desired datetime in any Ymd H [MS] format or \code{POSIXct}
#' @param timezone Timezone in which to interpret the \code{datetime}.
#' @param bbox Bounding box for the region of interest.
#' @param fullDay Logical specifying whether to list files for an entire 
#' local time day.
#' @param daylightOnly Logical specifying whether to list only those files
#' containing daylight imagery somewhere over the region of interest specified
#' by \code{bbox}.
#' @param useLocalDir Logical specifying whether to look for files in 
#' \code{getSatelliteDataDir()} or \code{baseUrl}.
#' @param baseUrl Base URL for data queries.
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
#' @seealso \link{goesaodc_listLocalFiles}
#' @seealso \link{goesaodc_listRemoteFiles}
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' noon <- "2019-09-06 12:00"
#' 
#' # Default to noon UTC
#' goesaodc_listFiles(
#'   satID = "G16", 
#'   datetime = datetime
#' )
#'
#' # Noon on the west coast
#' goesaodc_listFiles(
#'   satID = "G16", 
#'   datetime = datetime,
#'   timezone = "America/Los_Angeles"
#' )
#'
#' # Full local time day (daylight anywhere in the US)
#' goesaodc_listFiles(
#'   satID = "G16", 
#'   datetime = datetime,
#'   timezone = "America/Los_Angeles",
#'   fullDay = TRUE
#' )
#'
#' CA_bbox <- c(-125, -114, 32, 42)
#' 
#' # Full local time day (daylight only in California)
#' goesaodc_listFiles(
#'   satID = "G16", 
#'   datetime = datetime,
#'   timezone = "America/Los_Angeles",
#'   fullDay = TRUE,
#'   bbox = CA_bbox
#' )
#'
#' }

goesaodc_listFiles <- function(
  satID = NULL,
  datetime = NULL,
  timezone = "UTC",
  bbox = c(-125, -65, 24, 50), # CONUS
  fullDay = FALSE,
  daylightOnly = TRUE,
  useLocalDir = TRUE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(bbox)
  MazamaCoreUtils::stopIfNull(baseUrl)
  
  satID <- toupper(satID)
  
  # Create satUrl
  if ( !useLocalDir ) {
    if ( satID == "G16" ) {
      satUrl <- paste0(baseUrl, "GOES-16/AODC")
    } else if ( satID == "G17" ) {
      satUrl <- paste0(baseUrl, "GOES-17/AODC")
    } else {
      stop("Parameter 'satID' must be either 'G16' or 'G17'")
    }
  }
  
  # ----- Create startPatterns -------------------------------------------------
  
  # Create a vector of POSIXct hours
  
  if ( !fullDay ) {
    hours <- 
      MazamaCoreUtils::parseDatetime(datetime, timezone) %>%
      lubridate::floor_date(unit = "hour")
  } else {
    startHour <- 
      MazamaCoreUtils::parseDatetime(datetime, timezone) %>%
      lubridate::floor_date(unit = "day")
    endHour <- startHour + lubridate::hours(24)
    hours <- seq(startHour, endHour, by = "hour")
  }
  
  # Mask for daylight hours
  
  if ( daylightOnly ) {
    daylightMask <- isDaylight(hours, timezone, bbox)
    hours <- hours[daylightMask]
  }
  
  # Create a vector of UTC startPatterns for each requested hour
  startPatterns <- c(strftime(hours, "_s%Y%j%H", tz = "UTC"))
  
  # ----- Assemble regex to search with ----------------------------------------
  regex <- paste0(
    "OR_ABI-L2-AODC-M[0-9]_",
    satID,
    "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  )
  
  # ----- Local or Remote ------------------------------------------------------
  
  if ( useLocalDir ) {
    
    dataFiles <- list.files(getSatelliteDataDir(), pattern = regex)
    
  } else {
    
    links <-
      xml2::read_html(satUrl) %>%
      xml2::xml_child("body") %>%
      xml2::xml_child("table") %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")
    
    # TODO:  Figure out which implementation is faster
    # dataFiles <-
    #   xml2::read_html(satUrl) %>%
    #   rvest::html_nodes("table") %>%
    #   rvest::html_nodes("a") %>% 
    #   rvest::html_text()
    
    dataFiles <- links[-(1:5)]
    
  }
  
  # ----- Find matching files --------------------------------------------------
  
  # Assemble a list of all satellite data files for our satID

  
  # TODO:  Figure out the proper functional programming way to do this using
  # TODO:  purrr or base::Map().
  
  indicesList <- list()
  for ( startPattern in startPatterns ) {
    indicesList[[startPattern]] <- 
      which(stringr::str_detect(dataFiles, startPattern))
  }
  indices <- as.numeric(unlist(indicesList))
  
  matchingFiles <- dataFiles[indices]
  
  return(matchingFiles)
  
} # END OF FUNCTION

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  satID <- "G16"
  datetime <- "2019-09-06 12:00"
  timezone <- "America/Los_Angeles"
  bbox <- c(-125, -65, 24, 50) # CONUS
  fullDay <- TRUE
  daylightOnly <- TRUE
  useLocalDir <- FALSE
  baseUrl <- "https://tools-1.airfire.org/Satellite/"
  
  goesaodc_listFiles(
    satID = satID,
    datetime = datetime,
    timezone = timezone,
    bbox = bbox,
    fullDay = fullDay,
    daylightOnly = daylightOnly,
    useLocalDir = useLocalDir,
    baseUrl = baseUrl
  ) 
  
}