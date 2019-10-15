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
#' @param baseUrl Base URL for data queries.
#' 
#' @description Retrieve a list of GOES AOD files available in the
#' \code{satelliteDataDir} for a specified date and hour.
#' 
#' This function is a wrapper for 
#' \preformatted{
#' files <- goesaodc_listFiles(
#'   satID = satID,
#'   datetime = datetime,
#'   timezone = timezone,
#'   bbox = bbox,
#'   fullDay = fullDay,
#'   daylightOnly = daylightOnly,
#'   useLocalDir = FALSE,
#'   baseUrl = baseUrl
#' ) 
#' }
#'
#' Note that all files for a particular hour will be returned even if the
#' incoming \code{startdate} or \code{jdate} is specified to the minute or
#' second.
#'
#' @return Vector of filenames.
#'
#' @seealso \link{goesaodc_listFiles}
#' @seealso \link{goesaodc_listLocalFiles}
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' noon <- "2019-09-06 12:00"
#' 
#' # Default to noon UTC
#' goesaodc_listRemoteFiles(
#'   satID = "G16", 
#'   datetime = datetime
#' )
#'
#' # Noon on the west coast
#' goesaodc_listRemoteFiles(
#'   satID = "G16", 
#'   datetime = datetime,
#'   timezone = "America/Los_Angeles"
#' )
#'
#' # Full local time day (daylight anywhere in the US)
#' goesaodc_listRemoteFiles(
#'   satID = "G16", 
#'   datetime = datetime,
#'   timezone = "America/Los_Angeles",
#'   fullDay = TRUE
#' )
#'
#' CA_bbox <- c(-125, -114, 32, 42)
#' 
#' # Full local time day (daylight only in California)
#' goesaodc_listRemoteFiles(
#'   satID = "G16", 
#'   datetime = datetime,
#'   timezone = "America/Los_Angeles",
#'   fullDay = TRUE,
#'   bbox = CA_bbox
#' )
#'
#' }

goesaodc_listRemoteFiles <- function(
  satID = NULL,
  datetime = NULL,
  timezone = "UTC",
  bbox = c(-125, -65, 24, 50), # CONUS
  fullDay = FALSE,
  daylightOnly = TRUE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(bbox)
  MazamaCoreUtils::stopIfNull(baseUrl)
  
  if ( !is.logical(fullDay) )
    fullDay <- FALSE
  
  if ( !is.logical(daylightOnly) )
    fullDay <- TRUE
  
  # ----- Call goesaodc_ListFiles() ---------------------------------------------
  
  files <- goesaodc_listFiles(
    satID = satID,
    datetime = datetime,
    timezone = timezone,
    bbox = bbox,
    fullDay = fullDay,
    daylightOnly = daylightOnly,
    useLocalDir = FALSE,
    baseUrl = baseUrl
  ) 
  
  return(files)
  
}