#' @export
#' 
#' @title goesaodc_createRasterStack()
#' 
#' @description Creates a \code{RasterStack} from GOES AOD data files for the
#' date and hour specified by \code{datetime}. If \code{endtime} is specified, 
#' all scans from \code{startime} up to (but not including) \code{endtime} will 
#' be included. These files will be downloaded if they do not exist in the the 
#' directory specified by \code{setSatelliteDataDir()}.
#'
#' The Z axis of the \code{RasterStack} is a character vector where each element
#' is the time stamp of the scan and has the format YYYYMMDDHHMMSS. This can be
#' accessed using the \code{raster::getZ()} function. Names of the
#' \code{RasterStack} are also time stamps of the scan in the format: XHH.MM.SS.
#'
#' The \code{bbox} parameter can be a vector of floats in c(lonLo, lonHi, latLo,
#' latHi) order or the return value of \code{sp::bbox()} or 
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
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param endtime End time in any Ymd H [MS] format or \code{POSIXct}
#' (exclusive).
#' @param var GOES data variable ("AOD, "DQF" or "ID"); Defaults to "AOD".
#' @param res Resolution of raster in degrees; Defaults to 0.1.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param isJulian Logical value determining whether \code{datetime} should be 
#' interpreted as a Julian date with day of year as a decimal number; Defaults 
#' to FALSE.
#' @param fileList Optional list of files to stack. Useful when working with 
#' custom time ranges. 
#' @param verbose Logical flag to print stacking progress; Defaults to FALSE.
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
#'   res = 0.075,
#'   bbox = kincade_bbox,
#'   dqfLevel = 3,
#'   timezone = "America/Los_Angeles",
#'   verbose = TRUE
#' )
#'
#' rasterVis::levelplot(rasterStack)
#' }

goesaodc_createRasterStack <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  var = "AOD",
  res = 0.1,
  bbox = bbox_CONUS,
  dqfLevel = NULL,
  timezone = "UTC",
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
  
  # Use timezone from POSIXt datetime, or passed in timezone
  if ( lubridate::is.POSIXt(datetime) ) {
    timezone <- lubridate::tz(datetime)
  } else {
    if ( !(timezone %in% OlsonNames()) ) {
      stop(sprintf("timezone \"%s\" is not recognized", timezone))
    }
  }
  
  # ----- Determine datetime range ---------------------------------------------
  
  datetime <- MazamaCoreUtils::parseDatetime(
    datetime = datetime,
    timezone = timezone,
    isJulian = isJulian
  )
  
  if ( !is.null(endtime) ) {
    endtime <- MazamaCoreUtils::parseDatetime(
      datetime = endtime, 
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
      endtime = endtime, 
      timezone = timezone, 
      verbose = verbose
    )
    
    # Read the required files
    fileList <- goesaodc_listFiles(
      satID = satID, 
      datetime = datetime, 
      endtime = endtime, 
      timezone = timezone
    )
  }
  
  # ----- Create RasterStack ---------------------------------------------------
  
  rasterStack <- raster::stack()
  nameList <- c()
  zList <- c()
  i <- 1
  
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
      
      if ( verbose ) {
        message(paste0("Stacked (", i, "/", length(fileList), "): ", nc_file))
      }
    }
    
    i <- i + 1
  }
  
  rasterStack <- raster::setZ(rasterStack, zList)
  names(rasterStack) <- nameList
  
  # ----- Return ---------------------------------------------------------------
  
  return(rasterStack)
  
}
