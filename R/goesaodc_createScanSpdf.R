#' @export
#' 
#' @title Create an SPDF from a GOES AOD scan
#' 
#' @description Creates a \code{SpatialPointsDataFrame} of AOD readings from a 
#' GOES scan file, or a list of \code{SpatialPointsDataFrame}s from a series of 
#' files.
#' 
#' @param filename Name of a scan file.
#' @param bbox Bounding box for the region of interest. All points outside of 
#' this area will be removed. Defaults to CONUS.
#' @param dqfLevel Allowed data quality level. All points with a DQF value
#' above this level will have their AOD values set to NA. Must be either 0, 1, 
#' 2, or 3, with 0 being the highest quality. Defaults to 3.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bboxOregon <- c(-125, -116, 42, 47)
#' 
#' scanFiles <- goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' # Create an SPDF from a single scan file
#' goesaodc_createScanSpdf(
#'   filename = scanFiles[1],
#'   bbox = bboxOregon,
#'   dqfLevel = 2
#' )
#' 
#' # Create a list of SPDFs from multiple scan files
#' goesaodc_createScanSpdf(
#'   filename = scanFiles,
#'   bbox = bboxOregon
#' )
#' 
#' # Create an empty SPDF from a faulty scan
#' goesaodc_createScanSpdf(
#'   filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
#'   bbox = bboxOregon
#' )
#' }

goesaodc_createScanSpdf <- function(
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(filename)
  
  if ( !(dqfLevel %in% c(0, 1, 2, 3)) )
    stop(paste0("Parameter 'dqfLevel' must be 0, 1, 2, or 3"))
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  result <- try({
    
    if ( length(filename) == 1 ) {
      
      # Create a single SPDF
      spdf <- goesaodc_createSingleScanSpdf(
        filename = filename,
        bbox = bbox,
        dqfLevel = dqfLevel
      )
      
      return(spdf)
      
    } else {
      
      filenames <- filename
      
      # Create a list of SPDFs
      spdfList <- list()
      for ( filename in filenames ) {
        
        scanLabel <- 
          filename %>%
          goesaodc_convertFilenameToDatetime() %>%
          MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
        
        spdf <- goesaodc_createSingleScanSpdf(
          filename = filename,
          bbox = bbox,
          dqfLevel = dqfLevel
        )
        
        spdfList[[scanLabel]] <- spdf
        
      }
      
      return(spdfList)
      
    }
    
  }, silent = TRUE)
  
  # If there was an error reading the scan file, create an SPDF filled with NA 
  # AOD and DQF values for the requested satellite
  if ( "try-error" %in% class(result) ) {
    warning(result, immediate. = TRUE)
    spdf <- goesaodc_createEmptyScanSpdf(filename = filename, bbox = bbox)
    return(spdf)
  }
  
}


#' @title Create an SPDF from a single GOES AOD scan
#' 
#' @description Creates a \code{SpatialPointsDataFrame} of AOD readings from a 
#' GOES scan file.
#' 
#' @param filename Name of the scan file.
#' @param bbox Bounding box for the region of interest. Defaults to CONUS.
#' @param dqfLevel Allowed data quality level. All readings with a DQF value
#' above this level will have their AOD values set to NA. Must be either 0, 1, 
#' 2, or 3, with 0 being the highest quality. Defaults to 3.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bboxOregon <- c(-125, -116, 42, 47)
#' 
#' scanFile <- goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:30",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' MazamaSatelliteUtils:::goesaodc_createSingleScanSpdf(
#'   filename = scanFile,
#'   bbox = bboxOregon
#' )
#' }

goesaodc_createSingleScanSpdf <- function(
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(filename)
  
  if ( !(dqfLevel %in% c(0, 1, 2, 3)) )
    stop(paste0("Parameter 'dqfLevel' must be 0, 1, 2, or 3"))
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  # Download the file if it isn't available locally
  if ( !(filename %in% list.files(getSatelliteDataDir())) ) {
    goesaodc_downloadScanFiles(filenames = filename)
  }
  
  nc <- goesaodc_openScanFile(filename)
  
  tbl <- goesaodc_createScanTibble(nc, bbox, dqfLevel)
  
  spdf <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
    data = dplyr::select(tbl, .data$AOD)
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(spdf)
  
}
