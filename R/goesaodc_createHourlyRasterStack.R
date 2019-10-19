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
#' bbox_us <- c(-125, -65, 24, 50) # CONUS
#' 
#' rstrStack <- goesaodc_createHourlyRasterStack(
#' satID = "G16", 
#' datetime = "2019-09-06 16", 
#' bbox = bbox_us,
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
#' satID = "G17", 
#' datetime = "2019-09-06 09",
#' timezone = "America/Los_Angeles", 
#' bbox = bbox_oregon,
#' res = 0.1,
#' dqfLevel = 2)
#' 
#' rasterVis::levelplot(rstrStack)
#' }

goesaodc_createHourlyRasterStack <- function(
  satID = NULL,
  datetime = NULL,
  var = "AOD",
  res = 0.1,
  bbox = c(-125, -65, 24, 50), # LIMIT TO CONUS BY DEFAULT
  dqfLevel = NULL,
  timezone = 'UTC'
) {
  
  # ---- Validate parameters ---------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  
  # ---- Check for a POSIXt timezone -------------------------------------------
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(datetime)[1] %in% time_classes ) {
    timezone <- attr(datetime,"tzone")
  }

  # ---- Download GOES AOD Files -----------------------------------------------
  goesaodc_downloadAOD(satID, datetime, timezone = timezone)
  
  # ---- Create lists of files, start times and names for layers ---------------
  fileList <- goesaodc_listFiles(satID = satID, 
                                 datetime = datetime, 
                                 timezone = timezone)
  timeList <- goesaodc_getStartTime(fileList)
  nameList <- strftime(timeList, format = "%H:%M:%S", tz = "UTC")
  
  # ---- Create List of of AOD raster layers for hour and region ---------------
  hourStack <- raster::stack()
  
  for (nc_file in fileList) {
    nc <- goesaodc_openFile(nc_file)
    
    goes_raster <- goesaodc_createRaster(nc, 
                                         res = res, 
                                         bbox = bbox, 
                                         dqfLevel = dqfLevel )
    aod_raster <- goes_raster[[var]]
    hourStack <- raster::stack(hourStack, aod_raster)
    
  }
  
  hourStack <- raster::setZ(hourStack, timeList)
  names(hourStack) <- nameList

  return(hourStack)
}
