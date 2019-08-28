#!/usr/local/bin/Rscript

# This Rscript generates a spatial points video for a region over a given duration.
#
# Test this script from the command line with:
#
# ./createHourlyVideo_exec.R -s 20190801 -q 2 -r Maine -o ~/Desktop/ -v TRUE

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
      c("-s","--startdate"), 
      default=NULL, 
      help="The date to start at [default=\"%default\"]"
    ),
    make_option(
      c("-e","--enddate"), 
      default=NULL, 
      help="The date to end at [default=\"%default\"]"
    ),
    make_option(
      c("-d","--duration"), 
      default=3, 
      help="The number of days to cover [default=\"%default\"]"
    ),
    make_option(
      c("-r","--region"), 
      default=NULL, 
      help="Which region to view [default=\"%default\"]"
    ),
    make_option(
      c("-q", "--dqfLevel"),
      default=2,
      help="<= data quality level filter [default=\"%default\"]"
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
  cat(paste0("createHourlyVideo ", VERSION, "\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if (opt$startdate == "") {
  stop("Must define a start date")
}

if (opt$region == "") {
  stop("Must define a region")
}

if (!dir.exists(opt$outputDir)) {
  stop(paste0("outputDir not found:  ", opt$outputDir))
}

if (!dir.exists(opt$logDir)) {
  stop(paste0("logDir not found:  ", opt$logDir))
}



library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)
library(magrittr)

setSatelliteDataDir("~/Data/Satellite/")
setSpatialDataDir("~/Data/Spatial")

# ------ Setup region ----------------------------------------------------------

regions <- list(
  a = c("Washington", "Oregon", "Idaho"),
  b = c("Montana", "Wyoming"),
  c = c("North Dakota", "South Dakota", "Minnisota"),
  d = c("Wisconsin", "Michigan"),
  e = c("New York", "Vermont", "New Hampshire", "Maine", "Massachusetts", 
        "Connecticut", "Rhode Island"),
  f = c("California", "Nevada"),
  g = c("Utah", "Colorado", "Arizona", "New Mexico"),
  h = c("Nebraska", "Iowa", "Kansas", "Missouri"),
  i = c("Illinois", "Indiana", "Ohio", "Kentucky", "Tennessee"),
  j = c("Pennsylvania", "New Jersey", "West Virginia", "Maryland", "Deleware", 
        "Virginia", "North Carolina"),
  k = c("Texas", "Oklahoma"),
  l = c("Arkansas", "Louisiana", "Mississippi", "Alabama"),
  m = c("Georgia", "South Carolina", "Florida")
)

# Select the region containing the named state
matchingRegions <- sapply(1:length(regions), 
                          function(i) any(regions[[i]] == opt$region))
if (length(which(matchingRegions == TRUE)) > 0) {
  states <- regions[[which(matchingRegions == TRUE)]]
  bbox <- maps::map("state", regions = states, fill = TRUE, plot = FALSE)$range
} else {
  bbox <- maps::map("state", regions = ".", fill = TRUE, plot = FALSE)$range
}

# Region central coordinates
centerLon <- mean(bbox[1:2])
centerLat <- mean(bbox[3:4])

# ----- Setup hours ------------------------------------------------------------

startdate <- lubridate::ymd(opt$startdate, tz = "UTC")
duration <- lubridate::hours(11)

startHour <- startdate + lubridate::hours(12)         # Sunrise on first day
endHour <- startHour + lubridate::hours(opt$duration) # Sunset on last day

# Generate a frame for each hour
hours <- seq.POSIXt(from = startHour, to = endHour, by = "hour")
hoursInfo <- PWFSLSmoke::timeInfo(hours, 
                                  longitude = centerLon, latitude = centerLat, 
                                  timezone = "UTC") 

# ----- Generate frames --------------------------------------------------------

frameNumber <- 1
for (hour in as.list(hours)) {
  
  if (opt$verbose) {
    print(paste("Generating", strftime(hour, format = "%Y-%m-%d %H", tz = "UTC")))
  }
  
  hourString <- strftime(hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  #downloadedFiles <- goesaodc_downloadAOD(hourString)
  hourFiles <- goesaodc_listFiles(hourString)
  ncHandles <- purrr::map(hourFiles, goesaodc_openFile)
  
  # ----- Quality filter and average AOD ---------------------------------------
  
  # List of vectors of DQF level logicals for each scan
  dqfMasks <- 
    purrr::map(ncHandles, ncdf4::ncvar_get, varid = "DQF") %>% 
    purrr::map(as.vector) %>%
    purrr::map(function(x) { x > opt$dqfLevel })
  
  # List of vectors of AOD values for each scan
  aodScans <- 
    purrr::map(ncHandles, ncdf4::ncvar_get, varid = "AOD") %>% 
    purrr::map(as.vector) 
  
  # Mask each AOD scan by its DQF mask
  for (i in 1:length(aodScans)) {
    aodScans[[i]][dqfMasks[[i]]] <- NA
  }
  
  # Average together the remaining AOD values from all of the scans 
  stackedAODScans <- do.call(rbind, aodScans)
  avgAODReadings <- colMeans(stackedAODScans, na.rm = FALSE)
  
  # ----- Project and subset spatial points ------------------------------------
  
  # Construct a tibble to hold projected lat/lon point coords and average AOD
  varList <- list()
  varList[["AOD"]] <- avgAODReadings
  varList[["lon"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$longitude )
  varList[["lat"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$latitude )
  
  tbl <-
    tibble::as_tibble(varList) %>%
    tidyr::drop_na()
  
  # Subset points by the region bounding box
  tbl <- dplyr::filter(tbl, 
                       lon >= bbox[1], lon <= bbox[2],
                       lat >= bbox[3], lat <= bbox[4])
  
  # ----- Draw frame -----------------------------------------------------------
  
  # Plot setup
  i <- stringr::str_pad(frameNumber, 3, 'left', '0')
  frameFileName <- paste0(i, ".png")
  frameFilePath <- file.path(tempdir(), frameFileName)
  breaks <- c(-3.0, -0.2, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5, 3.0)
  png(frameFilePath, width = 1280, height = 720, units = "px")
  layout(matrix(c(1, 2, 2), nrow = 1, ncol = 3, byrow = TRUE))
  
  # Clock plot on the left
  plot(x = 1:10, y = 1:10, col = "transparent", axes = FALSE)
  title(paste(hourString, "UTC"), cex.main = 3.0)
  
  # Spatial points plot on the right
  maps::map("state", regions = states)
  if (nrow(tbl) > 0) {
    sp <- sp::SpatialPointsDataFrame(
      coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
      data = dplyr::select(tbl, -c(.data$lon, .data$lat))
    )
    goesaodc_plotSpatialPoints(sp, var = "AOD", cex = 0.5, breaks = breaks, 
                               add = TRUE)
    maps::map("state", regions = states, add = TRUE)
  }
  
  frameNumber <- frameNumber + 1
  dev.off()
}

# ----- Stitch together video --------------------------------------------------

videoFileName <- paste0(opt$region, "_AOD_DQF", opt$dqfLevel, 
                        ".mp4")

# Define system calls to ffmpeg to create video from frames
cmd_cd <- paste0("cd ", tempdir())
cmd_ffmpeg <- paste0("ffmpeg -loglevel quiet -r ", 
                     3, " -f image2 -s 1280x720 -i ",
                     "%03d.png -vcodec libx264 -crf 25 ", 
                     "~/Desktop", "/", videoFileName)
cmd_rm <- paste0("rm *.png")
cmd <- paste0(cmd_cd, " && ", cmd_ffmpeg, " && ", cmd_rm)

# Make system calls
ffmpegString <- paste(capture.output(system(cmd)), collapse="\n")
