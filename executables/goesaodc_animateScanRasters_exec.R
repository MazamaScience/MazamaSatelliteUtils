#!/usr/local/bin/Rscript

# This executable script generates a video of GOES AOD scan rasters over a time 
# range.
#
# Test this script from the command line with:
#
# ./goesaodc_animateScanRasters_exec.R
#  --satID="G17"
#  --starttime="2020-09-08 12:00"
#  --endtime="2020-09-08 13:00"
#  --timezone="America/Los_Angeles"
#  --bbox="c-125, -116, 42, 47"
#  --cellSize=0.1
#  --legendLimits="-0.5, 5.5"
#  --stateCodes="OR" 
#  --satelliteDataDir="~/Data/Satellite"
#  --spatialDataDir="~/Data/Spatial" 
#  --outputDir="~/Desktop"
#  --verbose TRUE

# ./goesaodc_animateScanRasters_exec.R --satID="G17" --starttime="2020-09-08 12:00" --endtime="2020-09-08 13:00" --timezone="America/Los_Angeles" --bbox="-125, -116, 42, 47" --cellSize=0.1 --legendLimits="-0.5, 5.5" --stateCodes="OR" --satelliteDataDir="~/Data/Satellite" --spatialDataDir="~/Data/Spatial" --outputDir="~/Desktop/" --verbose="TRUE"

VERSION = "0.1.0"

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
    satID = "G17",
    starttime = "2020-09-08 12:00",
    endtime = "2020-09-08 13:00",
    timezone = "America/Los_Angeles",
    bbox = "-125, -116, 42, 47",
    dqfLevel = 3,
    cellSize = 0.1,
    rasterAlpha = 0.6,
    paletteName = "YlOrRd",
    paletteBreaks = NULL,
    legendLimits = "-0.5, 5.5",
    includeMap = TRUE,
    zoom = 7,
    stateCodes = "OR",
    satelliteDataDir = "~/Data/Satellite",
    spatialDataDir = "~/Data/Spatial",
    outputDir = "~/Desktop",
    logDir = getwd(),
    outputFilename = NULL,
    frameRate = 6,
    verbose = TRUE,
    version = FALSE
  )
  
} else {
  
  # Set up OptionParser
  library(optparse)
  
  option_list <- list(
    
    make_option(
      c("--satID"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--starttime"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--endtime"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--timezone"),
      type = "character",
      default = "UTC",
      help = ""
    ),
    make_option(
      c("--bbox"),
      type = "character",
      default = bbox_CONUS,
      help = ""
    ),
    make_option(
      c("--dqfLevel"),
      type = "integer",
      default = 3,
      help = ""
    ),
    make_option(
      c("--cellSize"),
      type = "double",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--rasterAlpha"),
      type = "double",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--paletteName"),
      type = "character",
      default = "YlOrRd",
      help = ""
    ),
    make_option(
      c("--paletteBreaks"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--legendLimits"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--includeMap"),
      type = "logical",
      default = FALSE,
      help = ""
    ),
    make_option(
      c("--zoom"),
      type = "integer",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--stateCodes"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--satelliteDataDir"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--spatialDataDir"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--outputDir"),
      type = "character",
      default = getwd(),
      help = ""
    ),
    make_option(
      c("--logDir"),
      type = "character",
      default = getwd(),
      help = ""
    ),
    make_option(
      c("--outputFilename"),
      type = "character",
      default = NULL,
      help = ""
    ),
    make_option(
      c("--frameRate"),
      type = "double",
      default = 4,
      help = ""
    ),
    make_option(
      c("--verbose"),
      type = "logical",
      default = FALSE,
      help = ""
    ),
    make_option(
      c("--version"),
      action = "store_true",
      type = "logical",
      default = FALSE,
      help = ""
    )
  )
  
  # Parse arguments
  opt <- parse_args(OptionParser(option_list = option_list))
}

# Print out version and quit
if (opt$version) {
  cat(paste0("goesaodc_animateScanRasters_exec.R", VERSION, "\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

MazamaCoreUtils::stopIfNull(opt$satID)

opt$satID <- toupper(opt$satID)
if ( !(opt$satID %in% c("G16", "G17")) )
  stop("Argument 'satID' must be either 'G16' or 'G17'")

MazamaCoreUtils::stopIfNull(opt$starttime)
MazamaCoreUtils::stopIfNull(opt$endtime)
MazamaCoreUtils::stopIfNull(opt$timezone)

if ( !opt$timezone %in% OlsonNames() )
  stop(sprintf("Argument 'timezone': \"%s\" is not recognized", opt$timezone))

if ( !dir.exists(opt$satelliteDataDir) )
  stop(paste0("Satellite data directory not found: ", opt$satalliteDataDir))

if ( !dir.exists(opt$spatialDataDir) )
  stop(paste0("Spatial data directory not found: ", opt$spatialDataDir))

if ( !dir.exists(opt$outputDir) )
  stop(paste0("outputDir not found: ", opt$outputDir))

if ( !dir.exists(opt$outputDir) )
  stop(paste0("outputDir not found: ", opt$outputDir))

if ( opt$frameRate < 0 || opt$frameRate != floor(opt$frameRate) )
  stop("Argument 'frameRate' must be a positive integer")

if ( !dir.exists(opt$outputDir) )
  stop(paste0("outputDir not found: ", opt$outputDir))

if ( !dir.exists(opt$logDir) )
  stop(paste0("logDir not found:", opt$logDir))

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, "goesaodc_animateScanRasters_TRACE.log"),
  debugLog = file.path(opt$logDir, "goesaodc_animateScanRasters_DEBUG.log"),
  infoLog  = file.path(opt$logDir, "goesaodc_animateScanRasters_INFO.log"),
  errorLog = file.path(opt$logDir, "goesaodc_animateScanRasters_ERROR.log")
)

# For use at the very end
errorLog <- file.path(opt$logDir, "goesaodc_animateScanRasters_ERROR.log")

if ( interactive() )
  logger.setLevel(TRACE)

# Silence other warning messages
options(warn = -1) # -1 = ignore, 0 = save/print, 1 = print, 2 = error

# Start logging
logger.info("Running goesaodc_animateScanRasters_exec.R version %s", VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse = "\n")
logger.debug("R session:\n\n%s\n", sessionString)


# ----- Assemble data ----------------------------------------------------------

# Set data directories
setSpatialDataDir(opt$spatialDataDir)
setSatelliteDataDir(opt$satelliteDataDir)

# Load state polygons
loadSpatialData("USCensusStates")
loadSpatialData("NaturalEarthAdm1")

# Parse the starttime and endtime
starttime <- MazamaCoreUtils::parseDatetime(
  opt$starttime,
  timezone = opt$timezone
)

endtime <- MazamaCoreUtils::parseDatetime(
  opt$endtime,
  timezone = opt$timezone
)

# Parse the bbox vector
bbox <- as.numeric(unlist(strsplit(opt$bbox, ",")))
bbox <- bboxToVector(bbox)

# Parse the palette breaks
paletteBreaks <- if ( is.null(opt$paletteBreaks) ) {
  opt$paletteBreaks
} else {
  as.numeric(unlist(strsplit(opt$paletteBreaks, ",")))
}

# Parse the palette limits
legendLimits <- as.numeric(unlist(strsplit(opt$legendLimits, ",")))

# Parse the state codes
stateCodes <- if ( is.null(opt$stateCodes) ) {
  opt$stateCodes
} else {
  trimws(unlist(strsplit(opt$stateCodes, ",")))
}

if ( opt$verbose ) {
  logger.trace(
    "Animating scans from %s to %s",
    strftime(
      starttime,
      format = "%Y-%m-%d %H:%M:%S %Z",
      tz = attr(starttime, "tzone")
    ),
    strftime(
      endtime,
      format = "%Y-%m-%d %H:%M:%S %Z",
      tz = attr(endtime, "tzone")
    )
  )
}

logger.trace("Downloading scan files")

# Download scan files
scanFilenames <- goesaodc_downloadScanFiles(
  satID = opt$satID,
  datetime = starttime,
  endtime = endtime,
  timezone = opt$timezone,
  verbose = opt$verbose
)

# ----- Create frame files -----------------------------------------------------

videoTimeStamp <- MazamaCoreUtils::timeStamp(
  starttime,
  unit = "hour",
  timezone = opt$timezone
)

# Generate a frame for each scan file
frameNumber <- 0

for ( scanFilename in scanFilenames ) {
  
  # Frame setup
  frameNumber <- frameNumber + 1
  paddedFrameNumber <- stringr::str_pad(frameNumber, 3, 'left', '0')
  frameFilename <- paste0(videoTimeStamp, "_", paddedFrameNumber, ".png")
  frameFilePath <- file.path(tempdir(), frameFilename)
  
  # Create the plot title label
  localScanTime <- 
    scanFilename %>%
    goesaodc_convertFilenameToDatetime() %>%                # UTC time
    MazamaCoreUtils::parseDatetime(timezone = opt$timezone) # Local time
  
  timeString <- strftime(
    localScanTime,
    format = "%Y-%m-%d %H:%M:%S %Z",
    tz = attr(localScanTime, "tzone")
  )
  
  title <- paste0(
    ifelse(tolower(opt$satID) == "g16", "GOES-16", "GOES-17"),
    " AOD (DQF<=", opt$dqfLevel, ")",
    " for ", timeString
  )
  
  "GOES-16 AOD (DQF<=3) for ..."
  
  if (opt$verbose)
    logger.trace("Rendering frame for %s", timeString)
  
  # Create scan raster
  scanRaster <- goesaodc_createScanRaster(
    filename = scanFilename,
    bbox = bbox,
    dqfLevel = opt$dqfLevel,
    cellSize = opt$cellSize
  )
  
  # Draw plot
  scanPlot <- goesaodc_plotScanRaster(
    raster = scanRaster,
    bbox = bbox,
    rasterAlpha = opt$rasterAlpha,
    paletteName = opt$paletteName,
    paletteBreaks = paletteBreaks,
    legendLimits = legendLimits,
    includeMap = opt$includeMap,
    zoom = opt$zoom,
    stateCodes = stateCodes,
    title = title
  )
  
  # Save frame file
  ggplot2::ggsave(
    filename = frameFilename,
    plot = scanPlot,
    device = "png",
    path = tempdir(),
    width = 8,
    height = 4.5,
    dpi = 200
  )
  
}

# ----- Create video from frames -----------------------------------------------

videoFilename <- if ( is.null(opt$outputFilename) ) {
  paste0(
    "aod_",
    tolower(opt$satID), "_",
    "s", strftime(starttime, "%Y%m%d%H%M%S", "UTC"), "_",
    "e", strftime(endtime, "%Y%m%d%H%M%S", "UTC"), "_",
    "dqf", opt$dqfLevel,
    ".mp4"
  )
} else {
  ifelse(
    endsWith(".mp4", opt$outputFilename),
    opt$outputFilename,
    paste0(opt$outputFilename, ".mp4")
  )
}

videoFilePath <- file.path(opt$outputDir, videoFilename)

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

logger.info("Stitching frames into a video with ffmpeg")
logger.trace(cmd)

# Make system call
ffmpegString <- paste(capture.output({
  system(cmd)
}), collapse = "\n")

# ----- Finish -----------------------------------------------------------------

# Guarantee that an empty errorLog exists
if ( !file.exists(errorLog) )
  dummy <- file.create(errorLog)

logger.info("Completed successfully!")
