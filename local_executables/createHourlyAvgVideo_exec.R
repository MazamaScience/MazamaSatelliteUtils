#!/usr/local/bin/Rscript

# This Rscript generates an hourly aggregated AOD video of spatial points for a
# multi-state CONUS region.
#
# Test this script from the command line with:
#
# ./createHourlyAvgVideo_exec.R -s 20190801 -d 2 -r "california" -q 1 -n 12 -o ~/Desktop/
# ./createHourlyAvgVideo_exec.R --startdate="20190802" --duration="1" --region="New York" --dqfLevel="2" --naThreshold="2" --outputDir="~/Desktop/" --verbose="FALSE"

VERSION = "0.1.1"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(magrittr)
  library(MazamaCoreUtils)
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  library(optparse)
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(outputDir = getwd(),
              logDir = getwd(),
              version = FALSE)  
  
} else {
  
  # Set up OptionParser
  option_list <- list(
    make_option(
      c("-s","--startdate"), 
      default = NULL, 
      help = "The day to start on [default=\"%default\"]"
    ),
    make_option(
      c("-d","--duration"), 
      default = 1, 
      help = "The number of days to cover (including the start day) 
      [default=\"%default\"]"
    ),
    make_option(
      c("-r","--regionState"), 
      default = "conus", 
      help = "The full name of a state in the desired region. The center 
      coordinate of which is used to determine the video timezone 
      [default=\"%default\"]"
    ),
    make_option(
      c("-q", "--dqfLevel"),
      default = 2,
      help = "Lowest included AOD quality level (0=High, 1=Medium, 2=Low, 3=NA 
      DQF). All quality levels higher than this are displayed as well 
      [default=\"%default\"]"
    ),
    make_option(
      c("-i", "--satId"),
      default = NULL,
      help = "ID of the source GOES satellite (G16 or G17). Will be chosen 
      automatically for best coverage if not provided [default=\"%default\"]"
    ),
    make_option(
      c("-n", "--naThreshold"),
      default = 1,
      help = "Maximum allowable NA readings for a point in an hour (typically 12 
      readings per hour). Points exceeding this threshold are not displayed
      [default=\"%default\"]"
    ),
    make_option(
      c("-f", "--frameRate"),
      default = 2,
      help = "Video frames per second [default=\"%default\"]"
    ),
    make_option(
      c("-v","--verbose"), 
      default = TRUE, 
      help = "Print out generated frame files [default=\"%default\"]"
    ),
    make_option(
      c("-o","--outputDir"), 
      default = getwd(), 
      help = "Output directory for generated video file [default=\"%default\"]"
    ),
    make_option(
      c("-l","--logDir"), 
      default = getwd(), 
      help = "Output directory for generated .log file [default=\"%default\"]"
    ),
    make_option(
      c("-V","--version"), 
      action = "store_true", 
      default = FALSE, 
      help = "Print out version number [default=\"%default\"]"
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

opt$startdate <- lubridate::ymd(opt$startdate, tz = "UTC")
if (is.na(opt$startdate)) {
  stop("Start date must be in YYYYMMDD format")
}

opt$duration <- lubridate::hours(abs(as.integer(opt$duration) * 24 - 1))
if (is.na(opt$duration) || opt$duration < 1) {
  stop("Duration must be a positive integer")
}

opt$dqfLevel <- as.integer(opt$dqfLevel)
if (opt$dqfLevel < 0 || opt$dqfLevel > 3) {
  stop("DQF level must be 0, 1, 2, or 3")
}

if (!is.null(opt$satId)) {
  opt$satId <- toupper(opt$satId)
  if (!(opt$satId %in% c("G16", "G17"))) {
    stop("GOES satellite ID must be G16 or G17")
  }
}

opt$naThreshold <- as.integer(opt$naThreshold)
if (is.na(opt$naThreshold) || opt$naThreshold < 1) {
  stop("NA threshold must be a positive integer")
}

opt$frameRate <- as.integer(opt$frameRate)
if (is.na(opt$frameRate) || opt$frameRate < 1) {
  stop("Frame rate must be a positive integer")
}

if (!dir.exists(opt$outputDir)) {
  stop(paste0("outputDir not found:  ", opt$outputDir))
}
if (!endsWith(opt$outputDir, "/")) {
  opt$outputDir <- paste0(opt$outputDir, "/")
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
options(warn = -1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createHourlyAvgVideo_exec.R version %s", VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Setup region ----------------------------------------------------------

result <- try({
  
  setSatelliteDataDir("~/Data/Satellite/")
  setSpatialDataDir("~/Data/Spatial/")
  
  loadSpatialData("NaturalEarthAdm1")
  
  regions <- list(
    list(states = c("washington", "oregon", "idaho"),
         satId = "G17"),
    list(states = c("montana", "wyoming"),
         satId = "G17"),
    list(states = c("north dakota", "south dakota", "minnesota"),
         satId = "G16"),
    list(states = c("wisconsin", "michigan"),
         satId = "G16"),
    list(states = c("new york", "vermont", "new hampshire", "maine", 
                    "massachusetts", "connecticut", "rhode island"),
         satId = "G16"),
    list(states = c("california", "nevada"),
         satId = "G17"),
    list(states = c("utah", "colorado", "arizona", "new mexico"),
         satId = "G17"),
    list(states = c("nebraska", "iowa", "kansas", "missouri"),
         satId = "G16"),
    list(states = c("illinois", "indiana", "ohio", "kentucky", "tennessee"),
         satId = "G16"),
    list(states = c("pennsylvania", "new jersey", "west virginia", "maryland",
                    "deleware", "virginia", "north carolina"),
         satId = "G16"),
    list(states = c("texas", "oklahoma"),
         satId = "G16"),
    list(states = c("arkansas", "louisiana", "mississippi", "alabama"),
         satId = "G16"),
    list(states = c("georgia", "south carolina", "florida"),
         satId = "G16")
  )
  
  # Select the region containing the state parameter, defaulting to the whole 
  # CONUS
  opt$regionState <- tolower(opt$regionState)
  
  regionIndex <- which(
    sapply(regions, function(r) any(r$states == opt$regionState) )
  )
  
  if (length(regionIndex) == 1) {
    region <- regions[[regionIndex]]
    states <- region$states
    timeZoneState <- opt$regionState
  } else {
    states <- "."
    timeZoneState <- "colorado"
  }

  regionBbox <- 
    maps::map("state", regions = states, fill = TRUE, plot = FALSE)$range

  stateBbox <- 
    maps::map("state", 
              regions = c(timeZoneState), fill = TRUE, plot = FALSE)$range
  stateCenterLon <- mean(stateBbox[1:2])
  stateCenterLat <- mean(stateBbox[3:4])

  # Timezone is determined by the center of the regionState. The center of 
  # Florida though is in the Gulf so just that one case must be handled 
  # specially
  if (opt$regionState == "florida") {
    localTimezone <- "America/New_York"
  } else {
    localTimezone <- 
      MazamaSpatialUtils::getTimezone(lon = stateCenterLon, 
                                      lat = stateCenterLat,
                                      countryCodes = c("US"))
  }
  
  # Automatically select the best satellite for the region if the user did not
  # provided one
  if (is.null(opt$satId)) {
    opt$satId <- region$satId
  }
  
  # ----- Setup hours ----------------------------------------------------------
  
  # Assign local timezone to start date
  opt$startdate <- lubridate::ymd(opt$startdate, tz = localTimezone)
  
  # Convert start and end date to UTC
  startdateUTC <- lubridate::with_tz(opt$startdate, tzone = "UTC")
  enddateUTC <- startdateUTC + opt$duration
  
  # Get detailed local time info for all hours between the start and end
  localHours <- seq.POSIXt(from = startdateUTC, to = enddateUTC, by = "hour")
  localHoursInfo <- PWFSLSmoke::timeInfo(localHours, 
                                         longitude = stateCenterLon, 
                                         latitude = stateCenterLat, 
                                         timezone = localTimezone)
  
  # Keep only daylight hours and the midnight (00:00) hours between days
  if (FALSE) {
    localDaylightHours <- dplyr::filter(localHoursInfo, day == TRUE)
    localMidnightHours <- dplyr::filter(localHoursInfo[2:nrow(localHoursInfo), ], 
                                        lubridate::hour(localTime) == 0)
    localKeptHours <- 
      dplyr::arrange(rbind(localDaylightHours, localMidnightHours), localTime)
  
    # Convert the kept local hours to UTC
    frameTimeInfo <- lubridate::with_tz(localKeptHours, tzone = "UTC")
    frameHours <- frameTimeInfo$localTime
  }
  
  frameHours <- lubridate::with_tz(localHoursInfo$localTime, tzone = "UTC")
  
  # ----- Generate frames ------------------------------------------------------
  
  frameNumber <- 1
  for (hour in as.list(frameHours)) {
    
    localHour <- lubridate::with_tz(hour, tzone = localTimezone)
    localHourString <- strftime(localHour, format = "%Y-%m-%d %H", 
                                tz = localTimezone)
    
    utcHourString <- strftime(hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    logger.info("Generating frame for %s %s", localHourString, localTimezone)
    if (opt$verbose) {
      print(paste("Generating", localHourString, localTimezone))
    }
    
    # Fetch hour files if they are not already downloaded
    ncFiles <- goesaodc_listFiles(opt$satId, utcHourString)
    if (length(ncFiles) < 1) {
      logger.info("Downloading NetCDF files for %s", utcHourString)
      downloadedFiles <- goesaodc_downloadAOD(opt$satId, utcHourString)
      
      if (length(downloadedFiles) < 1) {
        stop(paste0("No ", opt$satId, " data for ", utcHourString))
      }
      
      ncFiles <- goesaodc_listFiles(opt$satId, utcHourString)
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
    varList[["AOD"]] <- 10 ^ avgAODReadings
    if (opt$satId == "G16") {
      varList[["lon"]] <- as.numeric(MazamaSatelliteUtils::goesEastGrid$longitude)
      varList[["lat"]] <- as.numeric(MazamaSatelliteUtils::goesEastGrid$latitude)
      satelliteName <- "GOES-East"
    } else if (opt$satId == "G17") {
      varList[["lon"]] <- as.numeric(MazamaSatelliteUtils::goesWestGrid$longitude)
      varList[["lat"]] <- as.numeric(MazamaSatelliteUtils::goesWestGrid$latitude)
      satelliteName <- "GOES-West"
    }
    aodTbl <- 
      tibble::as_tibble(varList) %>%
      tidyr::drop_na()
    
    # Subset points by the region bounding box
    aodTbl <- dplyr::filter(aodTbl, 
                         lon >= regionBbox[1], lon <= regionBbox[2],
                         lat >= regionBbox[3], lat <= regionBbox[4])
    
    # TODO: Make spatial points for readings with null AOD values or out of 
    # range DQF levels
  
    # ----- Draw frame ---------------------------------------------------------
    
    # Plot setup
    i <- stringr::str_pad(frameNumber, 4, 'left', '0')
    frameFileName <- paste0(i, ".png")
    frameFilePath <- file.path(tempdir(), frameFileName)
    breaks <- c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5)
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
    if (nrow(aodTbl) > 0) {
      sp <- sp::SpatialPointsDataFrame(
        coords = dplyr::select(aodTbl, c(.data$lon, .data$lat)),
        data = dplyr::select(aodTbl, -c(.data$lon, .data$lat))
      )
      goesaodc_plotSpatialPoints(sp, var = "AOD", cex = 0.5, breaks = breaks, 
                                 add = TRUE)
    }
    
    maps::map("state", regions = states, lwd = 2.0, add = TRUE)
    title(paste(satelliteName, " AOD, DQF <=", opt$dqfLevel), cex.main = 3.5)
    
    # Legend color scale
    paletteColors <- RColorBrewer::brewer.pal(length(breaks) - 1, "YlOrRd")
    legendImage <- as.raster(matrix(rev(paletteColors), ncol = 1))
    
    plot(0, 0, col = "transparent", xlim = c(-1, 1), 
         ylim = c(breaks[1], breaks[length(breaks)]), 
         axes = FALSE, xlab = NA, ylab = NA, main = "AOD", cex.main = 3.0)
    axis(side = 2, line = -6, at = breaks, 
         labels = sprintf(breaks, fmt = "%#.1f"), cex.axis = 2.0, las = 1)
    rasterImage(legendImage, -0.3, breaks[1], 0.3, breaks[length(breaks)],
                interpolate = FALSE)
    
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
  
  stateCode <- tolower(MazamaSpatialUtils::stateToCode(tools::toTitleCase(opt$regionState), "US"))
  videoFileName <- paste0(stateCode, "_aod_dqf", opt$dqfLevel, ".mp4")
  
  # Define system calls to ffmpeg to create video from frames
  cmd_cd <- paste0("cd ", tempdir())
  cmd_ffmpeg <- paste0("ffmpeg -loglevel quiet -r ", 
                       opt$frameRate, " -f image2 -s 1280x720 -i ",
                       "%04d.png -vcodec libx264 -crf 25 ", 
                       opt$outputDir, videoFileName)
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
