##' getDaylightHours.R
##' This is currently a stub that encapsulates the various logic that we have 
##' written to calculate the hours in which a location of interest is il-
##' luminated by the Sun, aka 'Daylight hours'.  A location of interest can be a 
##' timezone, a point location or an area defined by a bounding box.
##' 
##' SOME USEFUL EXAMPLES OF HOW THIS COULD BE USED
##' @param bbox Bounding box for the region of interest.
##' @param fullDay Logical specifying whether to list files for an entire 
##' local time day.
##' @param daylightOnly Logical specifying whether to list only those files
##' containing daylight imagery somewhere over the region of interest specified
##' by \code{bbox}.
##' 
##' library(MazamaSatelliteUtils)
##' setSatelliteDataDir("~/Data/Satellite")
##'
##' noon <- "2019-09-06 12:00"
##' 
##' # Default to noon UTC
##' goesaodc_listFiles(
##'   satID = "G16", 
##'   datetime = datetime
##' )
##'
##' # Noon on the west coast
##' goesaodc_listFiles(
##'   satID = "G16", 
##'   datetime = datetime,
##'   timezone = "America/Los_Angeles"
##' )
##'
##' # Full local time day (daylight anywhere in the US)
##' goesaodc_listFiles(
##'   satID = "G16", 
##'   datetime = datetime,
##'   timezone = "America/Los_Angeles",
##'   fullDay = TRUE
##' )
##'
##' CA_bbox <- c(-125, -114, 32, 42)
##' 
##' # Full local time day (daylight only in California)
##' goesaodc_listFiles(
##'   satID = "G16", 
##'   datetime = datetime,
##'   timezone = "America/Los_Angeles",
##'   fullDay = TRUE,
##'   bbox = CA_bbox
##' )
##' 
##' 
#fullDay <- TRUE
#daylightOnly <- TRUE
#
## ---- From old listFiles code -------------------------------------------------
#if ( !fullDay ) {
#  hours <- 
#    MazamaCoreUtils::parseDatetime(datetime, timezone) %>%
#    lubridate::floor_date(unit = "hour")
#} else {
#  startHour <- 
#    MazamaCoreUtils::parseDatetime(datetime, timezone) %>%
#    lubridate::floor_date(unit = "day")
#  endHour <- startHour + lubridate::hours(24)
#  hours <- seq(startHour, endHour, by = "hour")
#}
#
## Mask for daylight hours
#
#if ( daylightOnly ) {
#  daylightMask <- utils-time::isDaylight(hours, timezone, bbox)
#  hours <- hours[daylightMask]
#}
#
## ===== DEBUGGING ==============================================================
#
#if ( FALSE ) {
#  
#  satID <- "G16"
#  datetime <- "2019-09-06 12:00"
#  timezone <- "America/Los_Angeles"
#  bbox <- c(-125, -65, 24, 50) # CONUS
#  fullDay <- TRUE
#  daylightOnly <- TRUE
#  useLocalDir <- FALSE
#  baseUrl <- "https://tools-1.airfire.org/Satellite/"
#  
#  goesaodc_listFiles(
#    satID = satID,
#    datetime = datetime,
#    timezone = timezone,
#    bbox = bbox,
#    fullDay = fullDay,
#    daylightOnly = daylightOnly,
#    useLocalDir = useLocalDir,
#    baseUrl = baseUrl
#  ) 
#  
#}
