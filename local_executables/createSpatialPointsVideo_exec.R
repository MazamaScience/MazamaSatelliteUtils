#!/usr/local/bin/Rscript

# This Rscript generates a spatial points video of either AOD or DQF over a
# single CONUS state. The timelapse covers a single hour at ~5 minute intervals.
#
# Test this script from the command line with:
#
# ./createSpatialPointsVideo_exec.R -t 2019102715 -s CA -x AOD -q 2 \ 
# -r 2 -o ~/Desktop/ -v TRUE --SpatialDataDir="~/Data/Spatial" \ 
# --SatelliteDataDir="~/Data/Satellite" --bbox="-126,-119,30,40"

# ---- . ---- . auto aspect ratio
VERSION = "0.2.1"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(MazamaCoreUtils)
  library(MazamaSpatialUtils)
  library(MazamaSatelliteUtils)
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(
    SpatialDataDir="~/Data/Spatial",
    SatelliteDataDir="~/Data/Satellite",
    fullDay = TRUE,
    ##bbox = "-126,-119,34,40", # Kincade, extending out into Pacific
    bbox = "-124,-122,37,39", # Kincade, closeup
    datetime = "2019-10-27 14",       # Kincade fire near Sonoma, CA (local time)
    stateCode = "CA",
    satID = "G17",
    var = "AOD",
    dqfLevel = 2,
    frameRate = 5,
    verbose = TRUE,
    outputDir = getwd(),
    logDir = getwd(),
    version = FALSE
  )
  
} else {
  
  # Set up OptionParser
  library(optparse)
  
  option_list <- list(
    
    make_option(
      c("--SpatialDataDir"),
      default = NULL,
      help = "Defines SpatialDataDir location [SpatialDataDir = \"%default\"]"
    ),
    make_option(
      c("--SatelliteDataDir"),
      default = NULL,
      help = "Defines SatelliteDataDir location [SatelliteDataDir = \"%default\"]"
    ),
    make_option(
      c("--fullDay"),
      default = FALSE,
      help = "Sets whether period should be entire range of daytime hours [fullDay = \"%default\"]"
    ),
    make_option(
      c("--bbox"),
      default = NULL,
      help = "bbox argument as a string 'w,e,s,n' [bbox = \"%default\"]"
    ),
    make_option(
      c("-t", "--datetime"),
      default = NULL,
      help = "datetime of interest specified to the hour [default = \"%default\"]"
    ),
    
    make_option(
      c("-s", "--stateCode"),
      default = NULL,
      help = "Two-character state code [default = \"%default\"]"
    ),
    make_option(
      c("-x", "--var"),
      default = "AOD",
      help = "Variable displayed ('AOD' or 'DQF') [default = \"%default\"]"
    ),
    make_option(
      c("-q", "--dqfLevel"),
      default = 2,
      help = "Data quality filter level [default = \"%default\"]"
    ),
    make_option(
      c("-i", "--satID"),
      default = "G17",
      help = "Satellite ID [default = \"%default\"]"
    ),
    make_option(
      c("-r", "--frameRate"),
      default = 5,
      help = "Frames per second [default = \"%default\"]"
    ),
    make_option(
      c("-v","--verbose"),
      default = FALSE,
      help = "Print extra output [default = \"%default\"]"
    ),
    make_option(
      c("-o","--outputDir"),
      default = getwd(),
      help = "Output directory for generated video file [default = \"%default\"]"
    ),
    make_option(
      c("-l","--logDir"),
      default = getwd(),
      help = "Output directory for generated .log file [default = \"%default\"]"
    ),
    make_option(
      c("-V","--version"),
      action = "store_true",
      default = FALSE,
      help = "Print out version number [default = \"%default\"]"
    )
  )
  
  # Parse arguments
  opt <- parse_args(OptionParser(option_list = option_list))
}

# Print out version and quit
if (opt$version) {
  cat(paste0("createSpatialPointsVideo_exec.R ", VERSION, "\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

MazamaCoreUtils::stopIfNull(opt$datetime)

# TODO:  # Either bbox or stateCode must exist

# TO DO: Derive BBOX from State, if that's what's given

if ( opt$frameRate < 0 ||
     opt$frameRate != floor(opt$frameRate) )
  stop("Argument 'frameRate' must be a positive integer")

if ( !dir.exists(opt$outputDir) )
  stop(paste0("outputDir not found:  ", opt$outputDir))

if ( !dir.exists(opt$logDir) )
  stop(paste0("logDir not found:  ", opt$logDir))

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, "createSpatialPointsVideo_TRACE.log"),
  debugLog = file.path(opt$logDir, "createSpatialPointsVideo_DEBUG.log"),
  infoLog  = file.path(opt$logDir, "createSpatialPointsVideo_INFO.log"),
  errorLog = file.path(opt$logDir, "createSpatialPointsVideo_ERROR.log")
)

# For use at the very end
errorLog <- file.path(opt$logDir, "createSpatialPointsVideo_ERROR.log")

if ( interactive() )
  logger.setLevel(TRACE)

# Silence other warning messages
options(warn = -1) # -1 = ignore, 0 = save/print, 1 = print, 2 = error

# Start logging
logger.info("Running createSpatialPointsVideo_exec.R version %s", VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse = "\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ----- EVERYTHING inside an overarching try{} block ---------------------------

result <- try({
  
  # ----- Assemble data --------------------------------------------------------
  
  result <- try({
    
    # Set directories using command line parameters
    
    setSpatialDataDir(opt$SpatialDataDir)
    loadSpatialData("USCensusStates")
    setSatelliteDataDir(opt$SatelliteDataDir)
    
    # TODO:  # Use bbox if not NULL else state
    
    # Load state boundries
    state <- subset(USCensusStates, stateCode == opt$stateCode)
    
    # Parse out bbox vector
    bbox <- as.numeric(unlist(strsplit(opt$bbox, ",")))
    bbox <- bboxToVector(bbox)
    
    # Get the bbox components
    boundaries <- bboxToVector(bbox)
    w <- boundaries[1]
    e <- boundaries[2]
    s <- boundaries[3]
    n <- boundaries[4]
    
    mid_lon <- w + (e - w) / 2
    mid_lat <- s + (n - s) / 2
    
    # Adjust incoming bbox width to fit the video aspect ratio 1280/720
    videoAspectRatio <- 1280/720 # (width/height)
    newWidth <- (n - s) * videoAspectRatio
    w <- mid_lon - newWidth/2
    e <- mid_lon + newWidth/2
    bbox <- c(w, e, s, n)
    
    # Get the timezone in the bbox center
    timezone <- MazamaSpatialUtils::getTimezone(lon = mid_lon,
                                                lat = mid_lat,
                                                countryCodes = c("US"),
                                                useBuffering = TRUE)
    
    # TODO:  # Support full days if local time hour is midnight
    
    # Parse the incoming datetime
    datetime <-
      MazamaCoreUtils::parseDatetime(opt$datetime, timezone = timezone) %>%
      lubridate::floor_date(unit = "hour")
    
    videoTimeStamp <- MazamaCoreUtils::timeStamp(datetime, unit = "hour", timezone = timezone)
    
    if ( opt$verbose )
      logger.trace("Creating video for %s ...", strftime(datetime, "%Y-%m-%d %H:%M:%S %Z"))
    
    # Check whether to process an entire day
    if ( opt$fullDay ) {
      
      # Download the satellite scans for entire day
      scanFiles <- goesaodc_downloadDaytimeAOD(
        satID = opt$satID,
        datetime = datetime,
        timezone = timezone,
        verbose = opt$verbose
      )
      
    } else {
      
      # Download the satellite scans for this hour
      scanFiles <- goesaodc_downloadAOD(
        satID = opt$satID,
        datetime = datetime,
        endTime = NULL,
        timezone = timezone,
        isJulian = FALSE,
        verbose = opt$verbose
      )
      
    }
    
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    msg <- paste("Error assembling data: ", geterrmessage())
    logger.fatal(msg)
    stop(msg)
  }
  
  # ----- Create video frames --------------------------------------------------
  
  result <- try({
    
    # Generate a frame for each scan file
    frameNumber <- 0
    for ( scanFile in scanFiles ) {
      
      frameNumber <- frameNumber + 1
      
      # Frame setup
      i <- stringr::str_pad(frameNumber, 3, 'left', '0')
      frameFile <- paste0(videoTimeStamp, "_", i, ".png")
      frameFilePath <- file.path(tempdir(), frameFile)
      
      # Scan time
      scanTimeUTC <-goesaodc_convertFilenameToDatetime(scanFile)
      scanTimeLocal <- lubridate::with_tz(scanTimeUTC, tzone = timezone)
      
      # TODO: Get scanTimeLocal back into plot
      
      if ( opt$verbose )
        logger.trace("Start frame for %s", strftime(scanTimeLocal, "%H:%M:%S"))
      
      # Load scan data
      nc <- goesaodc_openFile(filename = scanFile)
      
      # Plot the frame
      png(frameFilePath, width = 1280, height = 720, units = "px")
      
      par(mar = c(0,0,0,0))
      
      if ( "try-error" %in% class(result) ) {
        
        # Error plot
        
        errMsg <- geterrmessage()
        
        # Still draw the state border even if there are no spatial points
        if ( grep("No data for selected region", errMsg) == 1 ) {
          par(bg = 'gray')
          plot(state, main = strftime(scanTimeLocal, "%Y-%m-%d %H:%M:%S %Z"))
        } else {
          stop(errMsg)
        }
        
      } else {
        
        # Data plot 
        # TODO: Update cex value based on bbox size 
        goesaodc_areaPlot(ncList = list(nc), 
                          bbox = bbox, 
                          dqfLevel = opt$dqfLevel)

        
      }
      
      if (opt$verbose)
        logger.trace("  End frame for %s", strftime(scanTimeLocal, "%H:%M:%S"))
      
      dev.off()
      
      if ( frameNumber > 12 ) 
        break
      
    }
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    msg <- paste("Error creating video frames: ", geterrmessage())
    logger.fatal(msg)
    stop(msg)
  }
  
  # ----- Create video ---------------------------------------------------------
  
  result <- try({
    
    videoFile <-
      paste0(state$stateCode, "_", videoTimeStamp, "_DQF", opt$dqfLevel, ".mp4")
    
    videoFilePath <- file.path(opt$outputDir, videoFile)
    
    # Define system calls to ffmpeg to create video from frames
    cmd_cd <- paste0("cd ", tempdir())
    cmd_ffmpeg <- paste0(
      "ffmpeg -loglevel quiet -r ",
      opt$frameRate, " -f image2 -s 1280x720 -i ",
      videoTimeStamp, "_%03d.png -vcodec libx264 -crf 25 ",
      videoFilePath
    )
    cmd_rm <- "rm *.png"
    cmd <- paste0(cmd_cd, " && ", cmd_ffmpeg, " && ", cmd_rm)

    
    logger.info("Calling ffmpeg to make video from frames")
    logger.trace(cmd)
    
    # Make system call
    ffmpegString <- paste(capture.output({
      system(cmd)
    }), collapse="\n")
    
    # TODO:  # Can additional flags generate output we can see?
    ###logger.trace("ffmpeg output:\n\n%s\n", ffmpegString)
    
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    msg <- paste("Error creating video frames: ", geterrmessage())
    logger.fatal(msg)
    stop(msg)
  }
  
  
}, silent=TRUE) # END outer try{} block

# ----- Handle errors ----------------------------------------------------------

if ( "try-error" %in% class(result) ) {
  
  msg <- paste("Error creating video: ", geterrmessage())
  logger.fatal(msg)
  
} else {
  
  # Guarantee that an empty errorLog exists
  if ( !file.exists(errorLog) )
    dummy <- file.create(errorLog)
  
  logger.info("Completed successfully!")
  
}
