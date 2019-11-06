#' @export
#'
#' @title List GOES AOD files that are available for a specific date and 
#' location during the hours between sunlight and sunset.  
#' 
#' @description List all daylight files available, either locally in directory
#' specified with \code{setSatelliteDir}, or optionally on remote file server 
#' by using the \code{useRemote} parameter.  See \code{getDaylightHours()} for 
#' details regarding how daylight hours are calculated.
#' 
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Specific date for which daylight hours are requested.
#' @param longitude Longitude of the location of interest in decimal degrees E.
#' @param latitude Latitude of the location of interest in decimal degrees N.
#' @param bbox Bounding box for the region of interest, Default: CONUS.
#' @param useRemote Logical specifying whether to look for files in 
#' \code{getSatelliteDataDir()} or \code{baseUrl}.
#' @param timezone Timezone in which to interpret the \code{datetime}.
#' @param isJulian Logical value determining whether datetime should be 
#' interpreted as a Julian date with day of year as a decimal number.
#' 
#' @return Vector of filenames.
#' 
#' @examples 
#' \donttest{
#' 
#' # List locally available files for a specific date by timezone
#' 
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' goesaodc_listDaytimeFiles(satID = "G16",
#' datetime = "2019-09-06", 
#' timezone = "America/Los_Angeles")
#' 
#' # List remote files available for specific date by timezone
#' 
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' goesaodc_listDaytimeFiles(satID = "G16", 
#' datetime = "2019-09-06", 
#' timezone = "America/Los_Angeles",
#' useRemote = TRUE)
#' }
#' 
#'
#' @rdname goesaodc_listDaytimeFiles
#' 

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
  
  # ---- Get the sunrise and sunset times --------------------------------------
  day_hours <- getDaylightHours(datetime = datetime,
                                timezone = timezone,
                                longitude = longitude,
                                latitude = latitude,
                                bbox = bbox,
                                isJulian = isJulian)
  
  sunrise <- day_hours$sunrise  # POSIXt return
  sunset <- day_hours$sunset    # POSIXt return
  
  # ---- Get available files that exist for period between sunrise and sunset --
  fileList <- goesaodc_listFiles(
    satID = satID,
    datetime = sunrise, 
    endTime = sunset,
    useRemote = useRemote)
  
  return(fileList)
}
