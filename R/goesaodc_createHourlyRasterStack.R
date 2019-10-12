#' @export
#' 
#' @title Create a RasterStack for a specified hour
#' 
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime desired datetime in any Ymd H [MS] format or \code{POSIXct}
#' @param var variable ("AOD, "DQF" or "ID")
#' @param res resolution of raster in degrees
#' @param bbox Bounding box for the region of interest.
#' @param dqfLevel Data quality flag level.
#' @param timezone timezone in which to interpret the \code{datetime}.
#' 
#' @description Create a \code{RasterStack} from GOES AOD data files for the 
#' date and hour specified by \code{datetime}. Each \code{RasterLayer} contains
#' data from one Advanced Baseline Imager (ABI) scan during the specified time 
#' period.
#' 
#' If data for the specified time period is not found in the directory specified 
#' by \code{setSatelliteDataDir()}, it will be downloaded in order to create the
#' \code{RasterStack}.
#' 
#' The Z axis of the \code{RasterStack} is a character vector where each element
#' is the time stamp of the scan and has the format YYYYMMDDHHMMSS. This can be 
#' accessed using the \code{raster::getZ()} function. Names of the 
#' \code{RasterStack} are also time stamps of the scan, of the format XHH.MM.SS.
#' 
#' The \code{dqfLevel} parameter can take a value of:
#'
#' \itemize{
#' \item{0}{ -- High quality retrieval flag}
#' \item{1}{ -- Medium quality retrieval flag}
#' \item{2}{ -- Low quality retrieval flag}
#' \item{3}{ -- No retrieval quality flag}
#' }
#' 
#' The \code{bbox} parameter can be a vector of floats in c(lonLo, lonHi, latLo,
#' latHi) order or the return value from \code{sp::bbox()} or 
#' \code{raster::extent()}.
#'
#' @return RasterStack
#' 
#' @examples
#' \donttest{
#' ### EXTENTS BASED ON PASSED IN FLOATS
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bbox_oregon <- c(-124.56624, -116.46350, 41.99179, 46.29203) # OREGON
#' 
#' rstrStack <- goesaodc_createHourlyRasterStack(
#' satID = "G16", 
#' datetime = "2019-09-06 16:00", 
#' bbox = bbox_oregon,
#' dqfLevel = 2,
#' res = 0.2)
#' 
#' rasterVis::levelplot(rstrStack)
#' 
#' #### EXTENTS BASED ON sp::bbox OF OREGON
#' library(MazamaSpatialUtils)
#' 
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusStates")
#' 
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' bbox_oregon <- sp::bbox(oregon)
#' 
#' rstrStack <- goesaodc_createHourlyRasterStack(
#' satId = "G17", 
#' datetime = "2019-09-06 16", 
#' bbox = bbox_oregon,
#' dqfLevel = 2)
#' 
#' rasterVis::levelplot(rstrStack)
#' }

goesaodc_createHourlyRasterStack <- function(
  satID = NULL,
  datetime = NULL,
  var = "AOD",
  res = 0.1,
  bbox = NULL,
  dqfLevel = NULL,
  timezone = 'UTC'
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) ) {
    stop("Parameter 'satID' must be either 'G16' or 'G17'")
  }
  
  # VALIDATE IS TIME BEING PASSED IN IS ALREADY A POSIX TIME WITH timezone
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(datetime)[1] %in% time_classes ) {
    timezone <- attr(datetime,"tzone")
  }
  
  # SINCE WE CAN PASS IN EITHER LOCAL OR UTC, MAKE SURE WE END UP WITH UTC
  datetime <- MazamaCoreUtils::parseDatetime(datetime, timezone)
  datetime <- lubridate::with_tz(datetime, tzone = "UTC")

  # ----- Download GOES AOD Files ----------------------------------------------
  
  # make sure that files are downloaded
  goesaodc_downloadAOD(satID, datetime)
  
  # ----- Create List of RasterLayers ------------------------------------------
  
  # create list of AOD raster layers for specified hour and region
  rasterList <- 
    goesaodc_listFiles(satID, datetime) %>%             # create list of filenames for specified hour
    purrr::map(goesaodc_openFile) %>%                   # open each file in the list
    purrr::map(goesaodc_createRaster,                   # rasterize each open file specified params
               res = res,
               bbox = bbox,                             
               dqfLevel = dqfLevel) %>%  
    purrr::map(function(rst) rst[[var]])                # select RasterLayer of specified variable
  
  # ----- Create RasterStack ---------------------------------------------------
  
  # Extents won't match exactly and raster::stack() needs them to, so
  # find the largest extent necessary to encompass all RasterLayers in the
  # list, and apply that new extent to each of the RasterLayers.
  lonLo <- min(purrr::map_dbl(rasterList, function(rst) raster::extent(rst)@xmin))
  lonHi <- max(purrr::map_dbl(rasterList, function(rst) raster::extent(rst)@xmax))
  latLo <- min(purrr::map_dbl(rasterList, function(rst) raster::extent(rst)@ymin))
  latHi <- max(purrr::map_dbl(rasterList, function(rst) raster::extent(rst)@ymax))
  
  ext <- raster::extent(c(lonLo, lonHi, latLo, latHi))
  
  rasterList <- purrr::map(rasterList, function(rst) raster::setExtent(rst, ext))
  
  # now create the stack
  rasterStack <- raster::stack(rasterList)
  
  # assign times to the Z axis
  times <-
    goesaodc_listFiles(satID, datetime) %>%
    purrr::map_chr(goesaodc_getStartString) %>%
    purrr::map(lubridate::parse_date_time, orders = ("YjHMS"))
  
  Z <- purrr::map(times, strftime, format = "%Y%m%d%H%M%S", tz = "UTC")
  
  rasterStack <- raster::setZ(rasterStack, Z)
  # NOTE: Why does this prepend "X" to the names?
  names <- purrr::map(times, strftime, format = "%H:%M:%S", tz = "UTC")
  names(rasterStack) <- names
  
  return(rasterStack)
  
}
