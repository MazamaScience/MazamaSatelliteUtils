#' @export
#' 
#' @title goesaodc_createRasterStack()
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Desired datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param endTime Desired ending time in any Ymd H [MS] format or 
#' \code{POSIXct}.
#' @param var GOES data variable ("AOD, "DQF" or "ID"); Defaults to "AOD".
#' @param res Resolution of raster in degrees; Defaults to 0.1.
#' @param bbox Bounding box for raster; Defaults to CONUS.
#' @param dqfLevel Data quality flag level.
#' @param timezone Timezone for \code{datetime} and optionally \code{endTime}.
#' @param isJulian Logical specifying if \code{datetime} (and optionally
#' \code{endTime}) are Julian formatted.
#' @param fileList Optional list of files to stack. Useful when working with 
#' custom time ranges. 
#' @param verbose Logical flag to print stacking information.
#'
#' @description  Create a \code{RasterStack} from GOES AOD data files for the
#' date and hour specified by \code{datetime}. Each \code{RasterLayer} contains
#' data from one Advanced Baseline Imager (ABI) scan during the specified time
#' period.
#'
#' If data for the given time period is not found in the directory specified by 
#' \code{setSatelliteDataDir()}, it will be downloaded in order to create the
#' \code{RasterStack}.
#'
#' The Z axis of the \code{RasterStack} is a character vector where each element
#' is the time stamp of the scan and has the format YYYYMMDDHHMMSS. This can be
#' accessed using the \code{raster::getZ()} function. Names of the
#' \code{RasterStack} are also time stamps of the scan in the format: XHH.MM.SS.
#'
#' The \code{bbox} parameter can be a vector of floats in c(lonLo, lonHi, latLo,
#' latHi) order or the return value from \code{sp::bbox()} or
#' \code{raster::extent()}.
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
#' @return RasterStack
#' 
#' @examples
#' \dontrun{
#' library(MazamaSatelliteUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' # Kincade fire region
#' kincade_bbox <- c(-124, -120, 36, 39)
#'
#' rasterStack <- goesaodc_createRasterStack(
#'   satID = "G17",
#'   datetime = "2019-10-27 10",
#'   endTime = "2019-10-27 11",
#'   res = 0.075,
#'   bbox = kincade_bbox,
#'   dqfLevel = 3,
#'   timezone = "America/Los_Angeles",
#'   verbose = TRUE
#' )
#'
#' rasterVis::levelplot(rasterStack)
#' }
#' 
#' @rdname goesaodc_createRasterStack

goesaodc_createRasterStack <- function(
  satID = NULL,
  datetime = NULL,
  endTime = NULL,
  var = "AOD",
  res = 0.1,
  bbox = c(-125, -65, 24, 50),
  dqfLevel = NULL,
  timezone = 'UTC',
  isJulian = FALSE,
  fileList = NULL,
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(dqfLevel)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) ) {
    stop("Must specify GOES satellite ID (G16 or G17)")
  }
  
  # Validate if time passed in is already a posix time with timezone
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(datetime)[1] %in% time_classes ) {
    timezone <- attr(datetime, "tzone")
  }
  
  # ----- Determine datetime range ---------------------------------------------
  
  datetime <- MazamaCoreUtils::parseDatetime(
    datetime = datetime,
    timezone = timezone,
    isJulian = isJulian
  )
  
  if ( !is.null(endTime) ) {
    endTime <- MazamaCoreUtils::parseDatetime(
      datetime = endTime, 
      timezone = timezone, 
      isJulian = isJulian
    )
  }
  
  # ----- Create nc file list --------------------------------------------------
  
  if ( is.null(fileList) ) {
    
    # Download GOES AOD files
    goesaodc_downloadAOD(
      satID = satID, 
      datetime = datetime, 
      endTime = endTime, 
      timezone = timezone, 
      verbose = verbose
    )
    
    # Read the required files
    fileList <- goesaodc_listFiles(
      satID = satID, 
      datetime = datetime, 
      endTime = endTime, 
      timezone = timezone
    )
  }
  
  # ----- Create RasterStack ---------------------------------------------------
  
  rasterStack <- raster::stack()
  nameList <- c()
  zList <- c()
  
  for (nc_file in fileList) {
    
    # ----- Create single Raster -----------------------------------------------
    
    time <- goesaodc_convertFilenameToDatetime(nc_file)
    name <- strftime(time, format = "%H:%M:%S", tz = "UTC")
    zValue <- strftime(time, format = "%Y%m%d%H%M%S", tz = "UTC")
    
    nc <- goesaodc_openFile(nc_file)
    
    # Create a raster from the .nc file
    result <- try({
      goes_raster <- goesaodc_createRaster(
        nc,
        res = res,
        bbox = bbox,
        dqfLevel = dqfLevel
      )
    }, silent = TRUE)
    
    if ( class(result) == "try-error" && 
         "No data for selected region" %in% attr(result, "condition")) {
      print(sprintf("No data present at %s UTC", name))
    } else {
      # Update the RasterStack name and z-value lists
      nameList <- append(nameList, name)
      zList <- append(zList, zValue)
      
      # Add this Raster to the RasterStack
      aod_raster <- goes_raster[[var]]
      rasterStack <- raster::stack(rasterStack, aod_raster)
      
      if ( verbose == TRUE ) {
        print(paste0("Stacked: ", nc_file))
      }
    }
  }
  
  rasterStack <- raster::setZ(rasterStack, zList)
  names(rasterStack) <- nameList
  
  # ----- Return ---------------------------------------------------------------
  
  return(rasterStack)
  
}
