#' @export
#'
#' @title Download GOES AOD data for the daylight hours of a specific date and 
#' location
#' 
#' @description Downloads a GOES satellite's daylight NetCDF files for the given 
#' \code{datetime} to the directory specified by \code{setSatelliteDataDir()}.
#' See \code{getDaylightHours()} for details regarding how daylight hours are 
#' calculated.
#' 
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param longitude Longitude of the location of interest in decimal degrees E.
#' @param latitude Latitude of the location of interest in decimal degrees N.
#' @param bbox Bounding box for the region of interest.
#' @param timezone Timezone used to interpret \code{datetime}.
#' @param isJulian Logical value determining whether \code{datetime} should be 
#' interpreted as a Julian date with day of year as a decimal number; Defaults 
#' to FALSE.
#' @param verbose Logical flag to print download progress; Defaults to FALSE.
#' 
#' @return Invisibly returns a vector of local files matching the requested 
#' \code{datetime}.
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' goesaodc_downloadDaytimeAOD(
#'   satID = "G16",
#'   datetime = "2019-09-06", 
#'   timezone = "America/Los_Angeles"
#' )
#' }

goesaodc_downloadDaytimeAOD <- function (
  satID = NULL,
  datetime = NULL,
  longitude = NULL,
  latitude = NULL,
  bbox = NULL,
  timezone = NULL,
  isJulian = FALSE,
  verbose = FALSE
) {
  
  # ---- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Must specify GOES satellite ID (G16 or G17)")
  
  # NOTE:  Additional parameter validation handled by downstream functions
  
  # ---- Get the sunrise and sunset times --------------------------------------
  
  day_hours <- getDaylightHours(
    datetime = datetime,
    longitude = longitude,
    latitude = latitude,
    bbox = bbox,
    timezone = timezone,
    isJulian = isJulian
  )
  
  sunrise <- day_hours$sunrise  # POSIXt return
  sunset <- day_hours$sunset    # POSIXt return
  
  # ---- Download files --------------------------------------------------------
  
  files <- goesaodc_downloadAOD(
    satID = satID,
    datetime = sunrise,
    endtime = sunset,
    verbose = verbose
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(files))
  
}
