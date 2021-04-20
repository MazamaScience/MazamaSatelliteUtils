#' @export
#' 
#' @title Create spatial points for a single scan
#' 
#' @description Creates a SpatialPointsDataFrame from a single scan file 
#' specified either by a filename or through satellite and time information.
#' 
#' @details If \code{filename} is given, \code{satID}, \code{datetime}, and
#' \code{timezone} will be ignored.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param filename The name of the scan file.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level; Defaults to 3.

goesaodc_singleScanToSPDF <- function(
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
  
  MazamaCoreUtils::stopIfNull(dqfLevel)
  
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
  
  sp <- goesaodc_createSpatialPoints(
    nc = nc,
    bbox = bbox,
    dqfLevel = dqfLevel
  )
    
  # ----- Return ---------------------------------------------------------------
  
  return(sp)
  
}

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  setSatelliteDataDir("~/Data/Satellite")
  
  oregon_bbox <- c(-125, -116, 42, 47)
  
  # Create points from a scan specified by satellite and datetime
  goesaodc_singleScanToSPDF(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = oregon_bbox,
    dqfLevel = 3
  )
  
  # Create points from a given scan file
  sp <- goesaodc_singleScanToSPDF(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
    bbox = oregon_bbox,
    dqfLevel = 3
  )
  
  # Draw plot
  goesaodc_plotSpatialPoints(sp, cex = 0.3)
  
  maps::map(
    database = "state",
    regions = "oregon",
    xlim = oregon_bbox[1:2],
    ylim = oregon_bbox[3:4],
    add  = TRUE
  )
  
}

