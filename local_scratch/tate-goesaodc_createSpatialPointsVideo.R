#' @export
#' 
#' @title Create a video timelapse of GOES spatial points
#' 
#' @param date the date (local)
#' @param state a SpatialPolygonsDataFrame for the state of interest
#' @param dqfLevel data quality flag level
#' 
#' @description Create a RasterBrick of GOES AOD data including data points
#' with the specified resolution and function, and within the specified extent 
#' and data quality flag level. Data quality level can take a value of:
#' 
#' 0: High quality retrieval flag
#' 1: Medium quality retrieval flag
#' 2: Low quality retrieval flag
#' 3: No retrieval quality flag
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' loadSpatialData("USCensusStates")
#' 
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' dateLocal <- lubridate::ymd("2019-08-01", tz = "America/Los_Angeles")
#' goesaodc_createSpatialPointsVideo(date = dateLocal, state = oregon, var = "AOD", dqfLevel = 2)
#' }

goesaodc_createSpatialPointsVideo <- function(
  date = NULL,
  state = NULL,
  var = "AOD",
  dqfLevel = 2
) {
  
  dateStr <- strftime(date, "%Y%m%d", tz = "America/Los_Angeles")
  bbox_state <- sp::bbox(state)
  
  # Calculate UTC start and end times
  #startTimeUTC <- date
  startTimeUTC <- date + lubridate::hours(6) # Debug start at sunrise
  attributes(startTimeUTC)$tzone <- "UTC"
  #endTimeUTC <- startTimeUTC + lubridate::hours(23)
  endTimeUTC <- startTimeUTC + lubridate::hours(11) # Short debug period
  
  # Get all the hours between the UTC start and end times
  hours <- seq.POSIXt(from = startTimeUTC, to = endTimeUTC, by = "hour")
  hours <- strftime(hours, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Run through each hour of the day, from 0 to 23
  frameNumber <- 1
  for (hour in hours) {
    
    print(hour)
    
    # Download the scan files for this hour
    goesaodc_downloadAOD(startdate = hour)
    
    # Get the names of these scan files
    scanFilenames <- goesaodc_listFiles(startdate = hour)
    
    # Generate a frame for each scan file
    for (scanFilename in scanFilenames) {
      
      # Load scan data
      nc <- goesaodc_openFile(filename = scanFilename)
      
      # Determine scan timestamp
      frameUTCTimestamp <- lubridate::parse_date_time(goesaodc_getStartString(scanFilename), orders = ("YjHMS"))
      frameLocalTimestamp <- frameUTCTimestamp
      attributes(frameLocalTimestamp)$tzone <- "America/Los_Angeles"
      
      # Try creating spatial points
      result <- try({
        sp <- goesaodc_createSpatialPoints(nc, bbox = bbox_state, dqfLevel = 2)
      }, silent = TRUE)
      
      # Frame setup
      i <- stringr::str_pad(frameNumber, 3, 'left', '0')
      frameFileName <- paste0(dateStr, "_", i, ".png")
      frameFilePath <- file.path(tempdir(), frameFileName)
      png(frameFilePath, width = 1280, height = 720, units = "px")
      
      if ("try-error" %in% class(result)) {
        errMsg <- geterrmessage()
        
        # Still draw the state border even if there are no spatial points
        if (grep("No data for selected region", errMsg) == 1) {
          par(bg = 'gray')
          plot(state, main = paste(frameLocalTimestamp, "PDT"))
        } else {
          stop(errMsg)
        }
      } else {
        par(bg = 'gray')
        plot(state, main =  paste(frameLocalTimestamp, "PDT"))
        goesaodc_plotSpatialPoints(sp,
                                   var = var,
                                   cex = 0.5, 
                                   breaks = c(-3.0, -0.2, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5, 3.0), 
                                   add = TRUE)
        plot(state, add = TRUE)
      }
      
      print(frameFileName)
      dev.off()
      frameNumber <- frameNumber + 1
    }
  }
  
  videoFileName <- paste0(state$stateCode, "_", dateStr, "_DQF", dqfLevel, ".mp4")
  
  # Define system calls to ffmpeg to create video from frames
  cmd_cd <- paste0("cd ", tempdir())
  cmd_ffmpeg <- paste0("ffmpeg -loglevel quiet -r 4 -f image2 -s 1280x720 -i ", dateStr, "_%03d.png -vcodec libx264 -crf 25 ~/Desktop/", videoFileName)
  cmd <- paste0(cmd_cd, " && ", cmd_ffmpeg)
  
  system(cmd)
  
}
