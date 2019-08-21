#!/usr/local/bin/Rscript

# This Rscript generates a spatial points video for a state over a given day. If
# The resulting video is labeled by the state, state, variable, and DQF level.
#
# Test this script from the command line with:
#
# ./createSpatialPointsVideo_exec.R -d 20190801 -s OR -x AOD -q 2 -r 4 -o ~/Desktop/ -v TRUE

VERSION = "0.1.1"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(MazamaCoreUtils)
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(outputDir = getwd(),
              logDir = getwd(),
              version = FALSE)  
  
} else {
  
  # Set up OptionParser
  library(optparse)
  
  option_list <- list(
    make_option(
      c("-s","--stateCode"), 
      default="", 
      help="The state's two-character state code [default=\"%default\"]"
    ),
    make_option(
      c("-d","--date"), 
      default=0, 
      help="The date [default=\"%default\"]"
    ),
    make_option(
      c("-x","--variable"), 
      default="AOD", 
      help="Which variable to color the spatial points for [default=\"%default\"]"
    ),
    make_option(
      c("-q", "--dqfLevel"),
      default=2,
      help="<= data quality level filter [default=\"%default\"]"
    ),
    make_option(
      c("-r", "--frameRate"),
      default=5,
      help="Frames per second [default=\"%default\"]"
    ),
    make_option(
      c("-v","--verbose"), 
      default=FALSE, 
      help="Print out generated frame files [default=\"%default\"]"
    ),
    make_option(
      c("-o","--outputDir"), 
      default=getwd(), 
      help="Output directory for generated video file [default=\"%default\"]"
    ),
    make_option(
      c("-l","--logDir"), 
      default=getwd(), 
      help="Output directory for generated .log file [default=\"%default\"]"
    ),
    make_option(
      c("-V","--version"), 
      action="store_true", 
      default=FALSE, 
      help="Print out version number [default=\"%default\"]"
    )
  )
  
  # Parse arguments
  opt <- parse_args(OptionParser(option_list=option_list))
}

# Print out version and quit
if (opt$version) {
  cat(paste0("createSpatialPointsVideo_exec.R ", VERSION, "\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if (opt$frameRate < 0 || opt$frameRate != floor(opt$frameRate)) {
  stop("frameRate must be a positive integer")
}

if (opt$date == "") {
  stop("Must define a date")
}

if (opt$state == "") {
  stop("Must define a state")
}

if (!dir.exists(opt$outputDir)) {
  stop(paste0("outputDir not found:  ", opt$outputDir))
}

if (!dir.exists(opt$logDir)) {
  stop(paste0("logDir not found:  ", opt$logDir))
}

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, "createSpatialPointsVideo_TRACE.log"),
  debugLog = file.path(opt$logDir, "createSpatialPointsVideo_DEBUG.log"), 
  infoLog  = file.path(opt$logDir, "createSpatialPointsVideo_INFO.log"), 
  errorLog = file.path(opt$logDir, "createSpatialPointsVideo_ERROR.log")
)

# For use at the very end
errorLog <- file.path(opt$logDir, "createSpatialPointsVideo_ERROR.log")

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createSpatialPointsVideo_exec.R version %s", VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create video frames ---------------------------------------------------

result <- try({
  
  # TODO: Set directories using command line parameters
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  loadSpatialData("USCensusStates")
  
  opt$date <- lubridate::ymd(opt$date, tz = "America/Los_Angeles")
  dateStr <- strftime(opt$date, "%Y%m%d", tz = "America/Los_Angeles")
  
  # Load state boundries
  state <- subset(USCensusStates, stateCode == opt$stateCode)
  bbox_state <- sp::bbox(state)
  
  # Calculate UTC start and end times
  startTimeUTC <- opt$date
  attributes(startTimeUTC)$tzone <- "UTC"
  endTimeUTC <- startTimeUTC + lubridate::hours(23)
  
  # Get all the hours between the UTC start and end times
  hours <- seq.POSIXt(from = startTimeUTC, to = endTimeUTC, by = "hour")
  hours <- strftime(hours, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Run through each hour of the day, from 0 to 23
  frameNumber <- 1
  for (hour in hours) {
    
    localHour <- hour
    attributes(localHour)$tzone <- "America/Los_Angeles"
    
    if (opt$verbose) {
      print(paste(localHour))
    }
    
    # Download the scan files for this hour
    logger.info("Loading data file for %s", localHour)
    goesaodc_downloadAOD(startdate = hour)
    
    # Get the names of these scan files
    scanFilenames <- goesaodc_listFiles(startdate = hour)
    
    # Generate a frame for each scan file
    for (scanFilename in scanFilenames) {
      
      # Determine scan timestamp
      frameUTCTimestamp <- lubridate::parse_date_time(goesaodc_getStartString(scanFilename), orders = ("YjHMS"))
      frameLocalTimestamp <- frameUTCTimestamp
      attributes(frameLocalTimestamp)$tzone <- "America/Los_Angeles"
      
      # Frame setup
      i <- stringr::str_pad(frameNumber, 3, 'left', '0')
      frameFileName <- paste0(dateStr, "_", i, ".png")
      frameFilePath <- file.path(tempdir(), frameFileName)
      
      logger.info("Generating frame for %s", strftime(frameLocalTimestamp, "%H:%M:%S"))
      
      # Load scan data
      nc <- goesaodc_openFile(filename = scanFilename)
      
      # Try creating spatial points
      result <- try({
        sp <- goesaodc_createSpatialPoints(nc, 
                                           bbox = bbox_state, 
                                           dqfLevel = opt$dqfLevel)
      }, silent = TRUE)
      
      # Plot the frame
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
                                   var = opt$variable,
                                   cex = 0.5, 
                                   breaks = c(-3.0, -0.2, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5, 3.0), 
                                   add = TRUE)
        plot(state, add = TRUE)
      }
      
      if (opt$verbose) {
        print(strftime(frameLocalTimestamp, "%H:%M:%S"))
      }
      dev.off()
      frameNumber <- frameNumber + 1
    }
  }
  
  videoFileName <- paste0(state$stateCode, "_", dateStr, "_DQF", opt$dqfLevel, 
                          ".mp4")
  
  # Define system calls to ffmpeg to create video from frames
  cmd_cd <- paste0("cd ", tempdir())
  cmd_ffmpeg <- paste0("ffmpeg -loglevel quiet -r ", 
                       opt$frameRate, " -f image2 -s 1280x720 -i ",
                       dateStr, "_%03d.png -vcodec libx264 -crf 25 ", 
                       opt$outputDir, "/", videoFileName)
  cmd_rm <- paste0("rm *.png")
  cmd <- paste0(cmd_cd, " && ", cmd_ffmpeg, " && ", cmd_rm)
  
  # Make system calls
  logger.info("Calling ffmpeg to make video from frames")
  logger.trace(cmd)
  
  ffmpegString <- paste(capture.output(system(cmd)), collapse="\n")
  
  logger.trace("ffmpeg output:\n\n%s\n", ffmpegString)
  
}, silent=TRUE)

if (opt$verbose) {
  print(result)
}

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error creating video: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
  logger.error("No errors")
}
