#' @export
#'
#' @title Download GOES-16 or GOES-17 AOD data for the daylight hours of a 
#' specific date and location.
#' 
#' #' @description Download all daylight files available to directory specified 
#' with \code{setSatelliteDir}.  See \code{getDaylightHours()} for details 
#' regarding how daylight hours are calculated.
#' 
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Specific date for which daylight hours are requested.
#' @param longitude Longitude of the location of interest in decimal degrees E.
#' @param latitude Latitude of the location of interest in decimal degrees N.
#' @param bbox Bounding box for the region of interest, Default: CONUS.
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
#' goesaodc_downloadDaytimeAOD(satID = "G16",
#' datetime = "2019-09-06", 
#' timezone = "America/Los_Angeles")
#' }
#' 
#' @rdname goesaodc_downloadDaytimeAOD
#' 

goesaodc_downloadDaytimeAOD <- function (
  satID = NULL,
  datetime = NULL,
  longitude = NULL,
  latitude = NULL,
  bbox = NULL,
  timezone = NULL,
  isJulian = FALSE
) {
  
  # ---- Get the sunrise and sunset times --------------------------------------
  day_hours <- getDaylightHours(datetime = datetime,
                                longitude = longitude,
                                latitude = latitude,
                                bbox = bbox,
                                timezone = timezone,
                                isJulian = isJulian)
  
  sunrise <- day_hours$sunrise  # POSIXt return
  sunset <- day_hours$sunset    # POSIXt return
  
  # ---- Download files that exist for period between sunrise and sunset --
  
  goesaodc_downloadAOD(
    satID = satID,
    datetime = sunrise,
    endTime = sunset)
  
}
