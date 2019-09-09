#!/usr/local/bin/Rscript

# This Rscript generates an hourly averaged AOD spatial points video for a 
# region in the continental US.

# This Rscript generates a spatial points video of AOD over a multi-state CONUS 
# region. The readings are hourly averaged from the start to end date but 
# exclude all nighttime hours except for each midnight hour (00:00) between 
# days.
#
# Test this script from the command line with:
#
# ./createHourlyAvgVideo_exec.R -s 20190801 -d 2 -r maine -q 1 -n 12 -o ~/Desktop/ -v TRUE
# ./createHourlyAvgVideo_exec.R --startdate="20190802" --duration="1" --region="New York" --dqfLevel="2" --naThreshold="2" --outputDir="~/Desktop/" --verbose="TRUE"

VERSION = "0.1.1"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(magrittr)
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
      c("-d","--duration"), 
      default=1, 
      help="The number of days to cover [default=\"%default\"]"
    ),
    make_option(
      c("-r","--regionState"), 
      default=".", 
      help="A state in the desired region [default=\"%default\"]"
    ),
    make_option(
      c("-q", "--dqfLevel"),
      default=2,
      help="<= data quality level filter (0-3) [default=\"%default\"]"
    ),
    make_option(
      c("-n", "--naThreshold"),
      default=1,
      help="Maximum allowable NA values for a point to be averaged 
      [default=\"%default\"]"
    ),
    make_option(
      c("-v","--verbose"), 
      default=TRUE, 
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
  cat(paste0("createHourlyAvgVideo ", VERSION, "\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if (opt$startdate == "") {
  stop("Must define a start date")
}

if (opt$duration < 1) {
  stop("Duration must be 1 or more days")
}

if (opt$dqfLevel < 0 || opt$dqfLevel > 3) {
  stop("DQF level must be an integer between 0 and 3")
}

if (!dir.exists(opt$outputDir)) {
  stop(paste0("outputDir not found:  ", opt$outputDir))
}

if (!dir.exists(opt$logDir)) {
  stop(paste0("logDir not found:  ", opt$logDir))
}

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, "createHourlyAvgVideo_TRACE.log"),
  debugLog = file.path(opt$logDir, "createHourlyAvgVideo_DEBUG.log"), 
  infoLog  = file.path(opt$logDir, "createHourlyAvgVideo_INFO.log"), 
  errorLog = file.path(opt$logDir, "createHourlyAvgVideo_ERROR.log")
)

# For use at the very end
errorLog <- file.path(opt$logDir, "createHourlyAvgVideo_ERROR.log")

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createHourlyAvgVideo_exec.R version %s", VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Setup region ----------------------------------------------------------

result <- try({
  
  setSatelliteDataDir("~/Data/Satellite/")
  setSpatialDataDir("~/Data/Spatial/")
  
  regions <- list(
    a = c("washington", "oregon", "idaho"),
    b = c("montana", "wyoming"),
    c = c("north dakota", "south dakota", "minnesota"),
    d = c("wisconsin", "michigan"),
    e = c("new york", "vermont", "new hampshire", "maine", "massachusetts", 
          "connecticut", "rhode island"),
    f = c("california", "nevada"),
    g = c("utah", "colorado", "arizona", "new mexico"),
    h = c("nebraska", "iowa", "kansas", "missouri"),
    i = c("illinois", "indiana", "ohio", "kentucky", "tennessee"),
    j = c("pennsylvania", "new jersey", "west virginia", "maryland", "deleware", 
          "virginia", "north carolina"),
    k = c("texas", "oklahoma"),
    l = c("arkansas", "louisiana", "mississippi", "alabama"),
    m = c("georgia", "south carolina", "florida")
  )
  
  # Select the region containing the state parameter, defaults to the whole 
  # CONUS
  opt$regionsState <- tolower(opt$regionState)
  
  matchingRegions <- 
    sapply(1:length(regions), 
           function(i) any(regions[[i]] == opt$regionState))
  
  if (length(which(matchingRegions)) > 0) {
    states <- regions[[which(matchingRegions)]]
  } else {
    states <- "."
  }
  
  regionBbox <- 
    maps::map("state", regions = states, fill = TRUE, plot = FALSE)$range
  
  # State central coordinates
  stateBbox <- 
    maps::map("state", 
              regions = c(opt$regionState), fill = TRUE, plot = FALSE)$range
  stateCenterLon <- mean(stateBbox[1:2])
  stateCenterLat <- mean(stateBbox[3:4])
  
  # Timezone is determined by the center of the named state in the region
  localTimezone <- 
    MazamaSpatialUtils::getTimezone(lon = stateCenterLon, lat = stateCenterLat)
  
  # TODO: Determine which satellite has better coverage of this region
  
  # ----- Setup hours ----------------------------------------------------------
  
  # Define local start date and duration (days covered including startdate)
  startdate <- lubridate::ymd(opt$startdate, tz = localTimezone)
  duration <- lubridate::hours(as.numeric(opt$duration) * 24 - 1)
  
  # Convert start and end date to UTC
  startdateUTC <- lubridate::with_tz(startdate, tzone = "UTC")
  enddateUTC <- startdateUTC + duration
  
  # Get detailed local time info for all hours between the start and end
  localHours <- seq.POSIXt(from = startdateUTC, to = enddateUTC, by = "hour")
  localHoursInfo <- PWFSLSmoke::timeInfo(localHours, 
                                         longitude = stateCenterLon, 
                                         latitude = stateCenterLat, 
                                         timezone = localTimezone)
  
  # Keep only daylight hours and the midnight (00:00) hours between days
  localDaylightHours <- dplyr::filter(localHoursInfo, day == TRUE)
  localMidnightHours <- dplyr::filter(localHoursInfo[2:nrow(localHoursInfo), ], 
                                      lubridate::hour(localTime) == 0)
  localKeptHours <- 
    dplyr::arrange(rbind(localDaylightHours, localMidnightHours), localTime)
  
  # Convert the kept local hours to UTC
  frameTimeInfo <- lubridate::with_tz(localKeptHours, tzone = "UTC")
  frameHours <- frameTimeInfo$localTime
  
  # ----- Generate frames ------------------------------------------------------
  
  frameNumber <- 1
  for (hour in as.list(frameHours)) {
    
    localHour <- lubridate::with_tz(hour, tzone = localTimezone)
    localHourString <- strftime(localHour, format = "%Y-%m-%d %H", 
                                tz = localTimezone)
    
    utcHourString <- strftime(hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ncFiles <- goesaodc_listFiles("G16", utcHourString)   # TODO: Determine proper satellite
    
    logger.info("Generating frame for %s %s", localHourString, localTimezone)
    if (opt$verbose) {
      print(paste("Generating", localHourString, localTimezone))
    }
    
    # Fetch hour files if they are not already downloaded
    if (length(ncFiles) < 1) {
      logger.info("Downloading NetCDF files for %s", utcHourString)
      goesaodc_downloadAOD("G16", utcHourString)          # TODO: Determine proper satellite
      ncFiles <- goesaodc_listFiles("G16", utcHourString) # TODO: Determine proper satellite
    }
    
    ncHandles <- purrr::map(ncFiles, goesaodc_openFile)
  
    # ----- Quality filter -----------------------------------------------------
    
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
    
    # ----- Average AOD --------------------------------------------------------
  
    # Average together the remaining AOD values from all of the scans 
    stackedAODScans <- do.call(rbind, aodScans)
    
    # It is much faster to use colMeans once rather than averaging each column 
    # uniquely
    if (opt$naThreshold <= 0) {
      avgAODReadings <- colMeans(stackedAODScans, na.rm = FALSE)
    } else if (opt$naThreshold >= nrow(stackedAODScans)) {
      avgAODReadings <- colMeans(stackedAODScans, na.rm = TRUE)
    }  else {
      avgAODReadings <- replicate(ncol(stackedAODScans), NA)
      
      for (col in 1:ncol(stackedAODScans)) {
        v <- as.vector(stackedAODScans[, col])
        naCount <- length(which(is.na(v)))
        if (naCount <= opt$naThreshold) {
          avgAODReadings[col] <- mean(v, na.rm = TRUE)
        }
      }
    }
    
    # ----- Project spatial points ---------------------------------------------
  
    # Construct a tibble to hold projected lat/lon point coords and average AOD
    varList <- list()
    varList[["AOD"]] <- avgAODReadings
    varList[["lon"]] <- as.numeric(MazamaSatelliteUtils::goesEastGrid$longitude)
    varList[["lat"]] <- as.numeric(MazamaSatelliteUtils::goesEastGrid$latitude)
    
    # Remove any points with NA lat/lon/AOD values
    tbl <-
      tibble::as_tibble(varList) %>%
      tidyr::drop_na()
    
    # Subset points by the region bounding box
    tbl <- dplyr::filter(tbl, 
                         lon >= regionBbox[1], lon <= regionBbox[2],
                         lat >= regionBbox[3], lat <= regionBbox[4])
  
    # ----- Draw frame ---------------------------------------------------------
    
    # Plot setup
    i <- stringr::str_pad(frameNumber, 3, 'left', '0')
    frameFileName <- paste0(i, ".png")
    frameFilePath <- file.path(tempdir(), frameFileName)
    # quantile(tbl$AOD, seq(from = 0.0, to = 1.0, by = 0.2), na.rm = TRUE)
    breaks <- c(-2.5750, 0.0800, 0.1207, 0.1675, 0.2145, 0.2736, 0.3651, 0.5764, 
                2.4750)
    png(frameFilePath, width = 1280, height = 720, units = "px")
    par(xpd = NA)
    layout(matrix(c(1, 1, 1, 1, 2,
                    1, 1, 1, 1, 2, 
                    1, 1, 1, 1, 3), nrow = 3, ncol = 5, byrow = TRUE), 
           respect = TRUE)
    
    # Spatial points map plot
    # Have to convert to polygons to keep aspect ratio
    mp <- maps::map("state", regions = states, fill = TRUE, plot = FALSE)
    polys <- maptools::map2SpatialPolygons(mp, IDs = mp$names, 
                                    proj4string = CRS("+proj=longlat +datum=WGS84"))
    polyData <- data.frame(seq_len(length(polys)), row.names = names(polys))
    spdf <- SpatialPolygonsDataFrame(polys, data = polyData)
    plot(spdf, border = NA, bg = "gray90")
    
    # Plot points if there are any 
    if (nrow(tbl) > 0) {
      sp <- sp::SpatialPointsDataFrame(
        coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
        data = dplyr::select(tbl, -c(.data$lon, .data$lat))
      )
      goesaodc_plotSpatialPoints(sp, var = "AOD", cex = 0.5, breaks = breaks, 
                                 add = TRUE)
    }
    maps::map("state", regions = states, lwd = 2.0, add = TRUE)
    title(paste("GOES East AOD, DQF <=", opt$dqfLevel), cex.main = 3.5)
    
    # Legend color scale
    cols <- RColorBrewer::brewer.pal(length(breaks) - 1, "YlOrRd")
    col_i <- .bincode(seq(from = breaks[length(breaks)], to = breaks[1], 
                          by = -0.05), breaks)
    col_v <- cols[col_i]
    legendImage <- as.raster(matrix(col_v, ncol = 1))
    plot(0, 0, col = "transparent", xlim = c(-1, 1), 
         ylim = c(breaks[1], breaks[length(breaks)]), 
         axes = FALSE, xlab = NA, ylab = NA, main = "AOD", cex.main = 3.0)
    axis(side = 2, line = -6, cex.axis = 2.0)
    rasterImage(legendImage, -0.3, breaks[1], 0.3, breaks[length(breaks)])
    
    # Timestamp clock plot
    hourFraction <- lubridate::hour(localHour) / 24
    pie(x = c(hourFraction, 1/24, 1 - (hourFraction + 1 / 24)), 
        clockwise = TRUE, init.angle = 270, labels = NA, 
        col = c("white", "black", "white"))
    text(0, -1.0, "Midnight", cex = 2.0)
    title(paste0(strftime(localHour, "%Y-%m-%d", tz = localTimezone), "\n", 
                 strftime(localHour, "%H:%M", tz = localTimezone)), 
          cex.main = 3.0)
    
    frameNumber <- frameNumber + 1
    dev.off()
  }
  
  # ----- Generate video -------------------------------------------------------
  
  videoFileName <- paste0(opt$regionState, "_AOD_DQF", opt$dqfLevel, 
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
  logger.info("Calling ffmpeg: %s", cmd)
  logger.trace(cmd)
  
  ffmpegString <- paste(capture.output(system(cmd)), collapse="\n")
  
  logger.trace("ffmpeg output:\n\n%s\n", ffmpegString)
  
})

if (opt$verbose) {
  print(result)
}

# Handle errors
if ("try-error" %in% class(result)) {
  msg <- paste("Error creating video: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if (!file.exists(errorLog)) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
  logger.error("No errors")
}
