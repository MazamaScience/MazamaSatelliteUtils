#' @export
#'
#' @title List GOES AOD files available for a specific date and 
#' location during daytime hours  
#' 
#' @description Retrieves a list of GOES AOD files for the daylight hours of a 
#' given \code{datetime}, available either locally in the specified 
#' \code{setSatelliteDir} or on a remote server using the \code{useRemote} 
#' parameter. See \code{getDaylightHours()} for details regarding how daylight 
#' hours are calculated.
#' 
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param longitude Longitude of the location of interest in decimal degrees E.
#' @param latitude Latitude of the location of interest in decimal degrees N.
#' @param bbox Bounding box for the region of interest.
#' @param useRemote Logical specifying whether to look for files in 
#' \code{getSatelliteDataDir()} or \code{baseUrl}; Defaults to FALSE;
#' @param timezone Timezone used to interpret \code{datetime}.
#' @param isJulian Logical value determining whether \code{datetime} should be 
#' interpreted as a Julian date with day of year as a decimal number; Defaults 
#' to FALSE.
#' 
#' @return Vector of filenames.
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' # List local files available for specific date by timezone
#' goesaodc_listDaytimeFiles(
#'   satID = "G16",
#'   datetime = "2019-09-06", 
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' # List remote files available for specific date by timezone
#' goesaodc_listDaytimeFiles(
#'   satID = "G16", 
#'   datetime = "2019-09-06", 
#'   timezone = "America/Los_Angeles",
#'   useRemote = TRUE
#' )
#' }
#' 
#' @rdname goesaodc_listDaytimeFiles

goesaodc_listDaytimeFiles <- function(
  satID = NULL,
  datetime = NULL,
  longitude = NULL,
  latitude = NULL,
  bbox = NULL,
  useRemote = FALSE,
  timezone = NULL,
  isJulian = FALSE
) {
  
  # ----- Get the sunrise and sunset times -------------------------------------
  
  day_hours <- getDaylightHours(
    datetime = datetime,
    timezone = timezone,
    longitude = longitude,
    latitude = latitude,
    bbox = bbox,
    isJulian = isJulian
  )
  
  sunrise <- day_hours$sunrise  # POSIXt return
  sunset <- day_hours$sunset    # POSIXt return
  
  # ----- Get available files for period between sunrise and sunset ------------
  
  fileList <- goesaodc_listFiles(
    satID = satID,
    datetime = sunrise, 
    endtime = sunset,
    useRemote = useRemote
  )
  
  return(fileList)
}
