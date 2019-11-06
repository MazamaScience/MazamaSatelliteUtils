#' @export
#' 
#' @title goesaodc_createRasterStack()
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Desired datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param endTime Desired ending time in any Ymd H [MS] format or \code{POSIXct}
#' @param var GOES data variable ("AOD, "DQF" or "ID"). Default: "AOD"
#' @param res resolution of raster in degrees. Default: 0.1
#' @param bbox Bounding box for raster, Default: c(-125, -65, 24, 50) CONUS
#' @param dqfLevel Data quality flag level.
#' @param timezone timezone for \code{datetime} and optionally \code{endTime}.
#' @param isJulian Logical specifying if \code{datetime} (and optionally
#' \code{endTime}) are Julian formatted
#' @param fileList optional list of files to stack. Useful when working with 
#' custom time ranges. 
#'
#' @description  Create a \code{RasterStack} from GOES AOD data files for the
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
#' @examples
#' \dontrun{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' satID <- "G16"
#' datetime <- "2019-08-12 09:00"
#' endTime <- "2019-08-12 12:00"
#' bbox <- c(-124.56624, -116.46350, 41.99179, 46.29203) # Oregon
#' dqfLevel <- 2
#' timezone <- "America/Los_Angeles"
#'
#' rasterStack <- goesaodc_createRasterStack(
#' satID = satID,
#' datetime = datetime,
#' endTime = endTime,
#' bbox = bbox,
#' dqfLevel = dqfLevel,
#' timezone = timezone)
#'
#' rasterVis::levelplot(rasterStack)
#'  }
#' @rdname goesaodc_createRasterStack

goesaodc_createRasterStack <- function(
  satID = NULL,
  datetime = NULL,
  endTime = NULL,
  var = "AOD",
  res = 0.1,
  bbox = c(-125, -65, 24, 50), # LIMIT TO CONUS BY DEFAULT
  dqfLevel = NULL,
  timezone = 'UTC',
  isJulian = FALSE,
  fileList = NULL
) {
  # ---- Check that mandatory parameters are present ---------------------------
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(dqfLevel)
  
  # ---- Convert satID to uniform case -----------------------------------------
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Must specify GOES satellite ID (G16 or G17)")
  
  # ---- Check if a timezone has been passed in with a POSIXt ------------------
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(datetime)[1] %in% time_classes ) {
    timezone <- attr(datetime, "tzone")
  }
  
  # ---- Parse incoming times with MazamaCoreUtils -----------------------------
  datetime <- MazamaCoreUtils::parseDatetime(datetime = datetime,
                                             timezone = timezone,
                                             isJulian = isJulian)
  
  if ( !is.null(endTime) ) {
    endTime <- MazamaCoreUtils::parseDatetime(endTime, timezone, isJulian = isJulian)
  }
  
  # ---- Create list of nc files to process, if one isn't already passed in ----
  if ( is.null(fileList) ) {
    
    # ---- Download GOES AOD Files ---------------------------------------------
    goesaodc_downloadAOD(satID, datetime, endTime, timezone = timezone)
    
    # ---- Read the local SatelliteDir for files that match parameters ---------
    fileList <- goesaodc_listFiles(satID = satID, 
                                   datetime = datetime, 
                                   endTime = endTime, 
                                   timezone = timezone)
  }
  
  # ---- Create List of of AOD raster layers for hour and region ---------------
  rasterStack <- raster::stack()
  
  # ----- Empty name and Z-value lists to keep running track of data -----------
  nameList <- c()
  zList <- c()
  
  for (nc_file in fileList) {
    # ---- Create layer names and Z values -------------------------------------
    time <- goesaodc_convertFilenameToDatetime(nc_file)
    name <- strftime(time, format = "%H:%M:%S", tz = "UTC")
    zValue <- strftime(time, format = "%Y%m%d%H%M%S", tz = "UTC")
    
    nc <- goesaodc_openFile(nc_file)
    result <- try({
      # ---- Attempt to create a raster from the .nc file data -----------------
      goes_raster <- goesaodc_createRaster(nc,
                                           res = res,
                                           bbox = bbox,
                                           dqfLevel = dqfLevel )
    }, silent = TRUE)
    if ( class(result) == "try-error" && 
         "No data for selected region" %in% attr(result, "condition")) {
      print(sprintf("No data present at %s UTC", name))
    } else {
      # ---- Update the name and Z-value lists ---------------------------------
      nameList <- append(nameList, name)
      zList <- append(zList, zValue)
      # ---- Add to the rasterStack the raster created from this .nc file ------
      aod_raster <- goes_raster[[var]] # just keep the variable specified
      rasterStack <- raster::stack(rasterStack, aod_raster) # add to the stack
      print(paste0("Stacked: ", nc_file))
    }
  }
  # ---- Write the Z-values and names to the rasterStack ---------------------
  rasterStack <- raster::setZ(rasterStack, zList)
  names(rasterStack) <- nameList
  
  
  return(rasterStack)
  
}
