#' @export
#' 
#' @title Create an AOD points plot
#' 
#' @description Creates a plot of AOD points using GOES scans. If no 
#' \code{endtime} is given, then only the scan closest to \code{datetime} will
#' be used. If an \code{endtime} is provided, then the plotted points will be
#' the average of the scans in the given time range. If \code{filename} is 
#' given, just the points for that scan file will be drawn.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param endtime End time in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param filename The name of the scan file.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level; Defaults to 3.
#' @param breaks Vector of AOD values to use as palette breaks.
#' @param pointSize Size of plot points; Defaults to 0.5.

goesaodc_plotScanPoints <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3,
  breaks = NULL,
  pointSize = 0.5
) {
  
  # ----- Create spatial points ------------------------------------------------
  
  if ( is.null(endtime) ) {
    
    sp <- goesaodc_createSingleScanSPDF(
      satID = satID,
      datetime = datetime,
      timezone = timezone,
      filename = filename,
      bbox = bbox,
      dqfLevel = dqfLevel
    )
    
  } else {
    
    sp <- goesaodc_createMultiScanSPDF(
      satID = satID,
      datetime = datetime,
      endtime = endtime,
      timezone = timezone,
      bbox = bbox,
      dqfLevel = dqfLevel
    )
    
  }
  
  # ----- Plot spatial points --------------------------------------------------
  
  p <- goesaodc_plotScanSPDF(
    sp = sp,
    bbox = bbox,
    breaks = breaks
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(p)
  
}

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  
  loadSpatialData("NaturalEarthAdm1")
  
  bbox_oregon <- c(-125, -116, 42, 47)
  
  # Plot Oregon at 5:30pm on Sept. 8, 2020
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    dqfLevel = 3,
    pointSize = 0.5
  )
  
  # Plot a scan file from Sept. 8, 2020
  goesaodc_plotScanPoints(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
    bbox = bbox_oregon,
    dqfLevel = 3,
    pointSize = 0.5
  )
  
  # Plot Oregon from 12pm to 1pm on Sept. 8, 2020
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2020-09-08 12",
    endtime = "2020-09-08 13",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    dqfLevel = 3,
    pointSize = 0.5
  )
  
}