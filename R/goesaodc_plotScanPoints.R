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
#' @param pointSize Size of plot points; Defaults to 0.5.
#' @param pointShape Shape of the plot points (index); Defaults to 15 (filled 
#' square).
#' @param breaks Vector of AOD values to use as palette breaks.
#' @param paletteName The name of an RColorBrewer palette; Defaults to 'YlOrRd'.
#' @param stateCodes Codes of state outlines to draw.
#' @param title Title of the plot.

goesaodc_plotScanPoints <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3,
  pointSize = 0.5,
  pointShape = 15,
  breaks = NULL,
  paletteName = "YlOrRd",
  stateCodes = NULL,
  title = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # ----- Create spatial points ------------------------------------------------
    
  spdf <- goesaodc_createScanSPDF(
    satID = satID,
    datetime = datetime,
    endtime = endtime,
    timezone = timezone,
    filename = filename,
    bbox = bbox,
    dqfLevel = dqfLevel
  )
  
  # ----- Plot spatial points --------------------------------------------------
  
  stateLayer <- if ( is.null(stateCodes) ) {
    NULL
  } else {
    AirFirePlots::layer_states(stateCodes)
  }
  
  scanPlot <- 
    goesaodc_plotScanSPDF(
      spdf = spdf,
      bbox = bbox,
      pointSize = pointSize,
      pointShape = pointShape,
      breaks = breaks,
      paletteName = paletteName,
      title = title
    ) +
    stateLayer
    
  # ----- Return ---------------------------------------------------------------
  
  return(scanPlot)
  
}

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  
  loadSpatialData("NaturalEarthAdm1")
  
  bbox_oregon <- c(-125, -116, 42, 47)
  
  # Plot Oregon at 5:30pm on Sep. 8, 2020
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    stateCodes = "OR",
    title = "Oregon AOD at 5:30pm PDT on Sep. 8, 2020"
  )
  
  # Plot Oregon with a scan file from Sep. 8, 2020
  filename <- "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc"
  localTimeStr <-
    filename %>%
    goesaodc_convertFilenameToDatetime() %>%
    MazamaCoreUtils::parseDatetime(timezone = "America/Los_Angeles")
  
  goesaodc_plotScanPoints(
    filename = filename,
    bbox = bbox_oregon,
    breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    stateCodes = "OR",
    title = paste0("Oregon AOD for ", localTimeStr, " PDT"),
    dqfLevel = 2
  )
  
  # Plot Oregon from 12pm to 1pm on Sept. 8, 2020
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2020-09-08 12",
    endtime = "2020-09-08 13",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    stateCodes = "OR",
    title = "Oregon AOD from 12pm to 1pm PDT on Sept. 8, 2020"
  )
  
  # Plot a scan with NA AOD values
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2019-10-27 10:00",
    timezone = "America/Los_Angeles",
    bbox = c(-124, -120, 36, 39)
  )
  
  # Plot averaged scans with NA AOD values
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "2019-10-27 10:00",
    endtime = "2019-10-27 11:00",
    timezone = "America/Los_Angeles",
    bbox = c(-124, -120, 36, 39),
    stateCodes = "CA",
    title = "San Francisco AOD from 10am to 11am on Oct. 27, 2019"
  )
  
  # Plot a faulty file
  goesaodc_plotScanPoints(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
    bbox = bbox_oregon,
    stateCodes = "OR"
  )
  
  # Plot a non-existant scan
  goesaodc_plotScanPoints(
    satID = "G17",
    datetime = "1970-01-01 12:00",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon
  )
  
}
