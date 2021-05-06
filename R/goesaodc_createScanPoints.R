#' @export
#' 
#' @title Create AOD points from GOES scans
#' 
#' @description Creates a \code{SpatialPointsDataFrame} of AOD readings from a 
#' GOES scan, or a list of \code{SpatialPointsDataFrame}s from a series of GOES 
#' scans.
#' 
#' @details A single scan can be identified by either giving its 
#' \code{filename}, or by providing the \code{satID}, \code{datetime}, and 
#' \code{timezone} (which will be used to determine the closest matching scan). 
#' A series of scans cannot be specified using filenames. The satellite + time 
#' info must be given along with an \code{endtime} so that all scans starting at
#' \code{datetime} up to (but not including) \code{endtime} will be processed.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param endtime End time in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
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
#' # Create points for a scan specified by satellite + time
#' goesaodc_createScanPoints(
#'   satID = "G17",
#'   datetime = "2020-09-08 17:30",
#'   timezone = "America/Los_Angeles",
#'   bbox = bboxOregon,
#' )
#' 
#' # Create points for a scan file
#' goesaodc_createScanPoints(
#'   filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
#'   bbox = bboxOregon,
#'   dqfLevel = 2
#' )
#' 
#' # Create points for multiple scans over a time range
#' goesaodc_createScanPoints(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles",
#'   bbox = bboxOregon
#' )
#' 
#' # Create points for a faulty scan
#' goesaodc_createScanPoints(
#'   filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
#'   bbox = bboxOregon
#' )
#' }

goesaodc_createScanPoints <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(filename) ) {
    
    MazamaCoreUtils::stopIfNull(satID)
    
    satID <- toupper(satID)
    if ( !(satID %in% c("G16", "G17")) )
      stop("Parameter 'satID' must be either 'G16' or 'G17'")
    
    MazamaCoreUtils::stopIfNull(datetime)
    
    # Use timezone from POSIXct datetime, or the given timezone
    if ( lubridate::is.POSIXt(datetime) ) {
      timezone <- lubridate::tz(datetime)
    } else {
      MazamaCoreUtils::stopIfNull(timezone)
      if ( !timezone %in% OlsonNames() )
        stop(sprintf("timezone \"%s\" is not recognized", timezone))
    }
    
  }
  
  if ( !(dqfLevel %in% c(0, 1, 2, 3)) )
    stop(paste0("Parameter 'dqfLevel' must be 0, 1, 2, or 3"))
  
  # ----- Create spatial points ------------------------------------------------
  
  result <- try({
    
    if ( is.null(filename) && !is.null(endtime) ) {
      
      # Create a multi-scan spdf list
      startFilename <- goasaodc_findClosestScanFile(
        satID = satID,
        datetime = datetime,
        timezone = timezone
      )
      
      endFilename <- goasaodc_findClosestScanFile(
        satID = satID,
        datetime = endtime,
        timezone = timezone
      )
      
      satUrl <- paste0(
        "https://tools-1.airfire.org/Satellite/",
        ifelse(satID == "G16", "GOES-16/AODC/", "GOES-17/AODC/")
      )
      
      # Get all remote netCDF files
      links <-
        xml2::read_html(satUrl) %>%
        xml2::xml_child("body") %>%
        xml2::xml_child("table") %>%
        xml2::xml_find_all("//a") %>%
        xml2::xml_attr("href")
      
      allScanFiles <- links[ -(1:5) ]
      
      startIndex <- which(startFilename == allScanFiles)[1]
      endIndex <- which(endFilename == allScanFiles)[1] - 1 # Exclusive end bound
      
      filenames <- allScanFiles[startIndex:endIndex]
      
      spdfList <- list()
      
      for ( filename in filenames ) {
        
        scanLabel <- 
          filename %>%
          goesaodc_convertFilenameToDatetime() %>%
          MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
        
        spdf <- goesaodc_createSingleScanPoints(
          filename = filename,
          bbox = bbox,
          dqfLevel = dqfLevel
        )
        
        spdfList[[scanLabel]] <- spdf
        
      }
      
      return(spdfList)
      
    } else {
      
      spdf <- goesaodc_createSingleScanPoints(
        satID = satID,
        datetime = datetime,
        timezone = timezone,
        filename = filename,
        bbox = bbox,
        dqfLevel = dqfLevel
      )
      
      return(spdf)
      
    }
    
  }, silent = TRUE)
  
  # If there was an error reading the scan file, create an SPDF filled with NA 
  # AOD and DQF values for the requested satellite
  if ( "try-error" %in% class(result) ) {
    warning(result, immediate. = TRUE)
    spdf <- goesaodc_createEmptyScanPoints(satID, filename, bbox)
    return(spdf)
  }
  
}


#' @title Create AOD points from a single GOES scan
#' 
#' @description Creates a \code{SpatialPointsDataFrame} from a GOES scan.
#' 
#' @details A scan can be identified by either giving its \code{filename}, or by
#' providing the \code{satID}, \code{datetime}, and \code{timezone} (which will 
#' be used to determine the closest matching scan). 
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param filename The name of the scan file.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Allowed data quality level. All readings with a DQF value
#' above this level will have their AOD values set to NA. Must be either 0, 1, 
#' 2, or 3, with 0 being the highest quality. Defaults to 3.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' loadSpatialData("NaturalEarthAdm1")
#' 
#' bboxOregon <- c(-125, -116, 42, 47)
#' 
#' # Create points from a scan specified by satellite and time
#' goesaodc_createSingleScanPoints(
#'   satID = "G17",
#'   datetime = "2020-09-08 17:30",
#'   timezone = "America/Los_Angeles",
#'   bbox = bboxOregon
#' )
#' 
#' # Create points from a scan file
#' goesaodc_createSingleScanPoints(
#'   filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
#'   bbox = bboxOregon,
#'   dqfLevel = 2
#' )
#' }

goesaodc_createSingleScanPoints <- function(
  satID = NULL,
  datetime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(filename) ) {
    
    MazamaCoreUtils::stopIfNull(satID)
    
    satID <- toupper(satID)
    if ( !(satID %in% c("G16", "G17")) )
      stop("Parameter 'satID' must be either 'G16' or 'G17'")
    
    MazamaCoreUtils::stopIfNull(datetime)
    
    # Use timezone from POSIXct datetime, or passed in timezone
    if ( lubridate::is.POSIXt(datetime) ) {
      timezone <- lubridate::tz(datetime)
    } else {
      if ( !timezone %in% OlsonNames() ) {
        stop(sprintf("timezone \"%s\" is not recognized", timezone))
      }
    }
    
  }
  
  if ( !(dqfLevel %in% c(0, 1, 2, 3)) )
    stop(paste0("Parameter 'dqfLevel' must be 0, 1, 2, or 3"))
  
  # ----- Get scan file --------------------------------------------------------
  
  if ( is.null(filename) ) {
    
    # Find which scan file is closest to the requested time
    filename <- goasaodc_findClosestScanFile(
      satID = satID,
      datetime = datetime,
      timezone = timezone
    )
    
  }
  
  # Download the file if it isn't available locally
  if ( !(filename %in% list.files(getSatelliteDataDir())) ) {
    
    satUrl <- ifelse(satID == "G16", "GOES-16/AODC/", "GOES-17/AODC/")
    
    fileUrl <- paste0(
      "https://tools-1.airfire.org/Satellite/",
      satUrl,
      filename
    )
    
    filePath <- file.path(getSatelliteDataDir(), filename)
    
    utils::download.file(
      fileUrl,
      destfile = filePath, 
      quiet = TRUE, 
      method = "auto", 
      mode = "wb"
    )
    
  }
  
  # ----- Create spatial points ------------------------------------------------
  
  nc <- goesaodc_openFile(filename)
  
  tbl <- goesaodc_createTibble(nc, bbox, dqfLevel)
  
  spdf <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
    data = dplyr::select(tbl, .data$AOD)
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(spdf)
  
}
