#!/usr/local/bin/Rscript

# This Rscript generates a spatial points video for a region over a given 
# duration.
#
# Test this script from the command line with:
#
# ./createHourlyVideo_exec.R -s 20190801 -q 2 -r Maine -o ~/Desktop/ -v TRUE

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

# ------ Setup region ----------------------------------------------------------

setSatelliteDataDir("~/Data/Satellite/")
setSpatialDataDir("~/Data/Spatial/")

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

# Select the region containing the state parameter
matchingRegions <- sapply(1:length(regions), 
                          function(i) any(regions[[i]] == opt$region))
if (length(which(matchingRegions == TRUE)) > 0) {
  states <- regions[[which(matchingRegions == TRUE)]]
  regionBbox <- 
    maps::map("state", regions = states, fill = TRUE, plot = FALSE)$range
} else {
  regionBbox <- 
    maps::map("state", regions = ".", fill = TRUE, plot = FALSE)$range
}

# State central coordinates
stateBbox <- 
  maps::map("state", regions = c(opt$region), fill = TRUE, plot = FALSE)$range
stateCenterLon <- mean(stateBbox[1:2])
stateCenterLat <- mean(stateBbox[3:4])

# Timezone is determined by the exact center of the region
localTimezone <- 
  MazamaSpatialUtils::getTimezone(lon = stateCenterLon, lat = stateCenterLat)

# ----- Setup hours ------------------------------------------------------------

# Define local start date and duration (days covered including startdate)
startdate <- lubridate::ymd(opt$startdate, tz = localTimezone)
duration <- lubridate::hours(as.numeric(opt$duration) * 24 - 1)

# Convert start and end date to UTC
startdateUTC <- lubridate::with_tz(startdate, tzone = "UTC")
enddateUTC <- startdateUTC + duration

# Get detailed time info for all hours in between
localHours <- seq.POSIXt(from = startdateUTC, to = enddateUTC, by = "hour")
localHoursInfo <- PWFSLSmoke::timeInfo(localHours, 
                                       longitude = stateCenterLon, 
                                       latitude = stateCenterLat, 
                                       timezone = localTimezone)

# Keep only daylight hours and the midnight (00:00) hours between days
localDaylightHours <- dplyr::filter(localHoursInfo, day == TRUE)
localMidnightHours <- dplyr::filter(localHoursInfo[2:nrow(localHoursInfo),], 
                                    lubridate::hour(localTime) == 0)
localKeptHours <- dplyr::arrange(rbind(localDaylightHours, localMidnightHours), 
                                 localTime)

# Convert kept local hours to UTC
frameTimeInfo <- lubridate::with_tz(localKeptHours, tzone = "UTC")
frameHours <- frameTimeInfo$localTime

# ----- Generate frames --------------------------------------------------------

frameNumber <- 1
for (hour in as.list(frameHours)) {
  
  localHour <- lubridate::with_tz(hour, tzone = localTimezone)
  if (opt$verbose) {
    print(paste("Generating", 
                strftime(localHour, format = "%Y-%m-%d %H", tz = localTimezone),
                localTimezone))
  }
  
  hourString <- strftime(hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  hourFiles <- goesaodc_listFiles(hourString)
  
  # Fetch hour files if they are not already downloaded
  if (length(hourFiles) < 1) {
    goesaodc_downloadAOD(hourString)
    hourFiles <- goesaodc_listFiles(hourString)
  }
  
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
                       lon >= regionBbox[1], lon <= regionBbox[2],
                       lat >= regionBbox[3], lat <= regionBbox[4])

  # ----- Draw frame -----------------------------------------------------------
  
  # Plot setup
  i <- stringr::str_pad(frameNumber, 3, 'left', '0')
  frameFileName <- paste0(i, ".png")
  frameFilePath <- file.path(tempdir(), frameFileName)
  breaks <- c(-2.5750, 0.0800, 0.1207, 0.1675, 0.2145, 0.2736, 0.3651, 0.5764, 2.4750) # quantile(tbl$AOD, seq(from = 0.0, to = 1.0, by = 0.2), na.rm = TRUE)
  png(frameFilePath, width = 1280, height = 720, units = "px")
  par(xpd = NA)
  layout(matrix(c(1, 1, 1, 1, 2,
                  1, 1, 1, 1, 2, 
                  1, 1, 1, 1, 3), nrow = 3, ncol = 5, byrow = TRUE), respect = TRUE)
  
  # Spatial points map plot
  # Have to convert to polygons to keep aspect ratio
  mp <- maps::map("state", regions = states, fill = TRUE, plot = FALSE)
  polys <- maptools::map2SpatialPolygons(mp, IDs = mp$names, 
                                      proj4string = CRS("+proj=longlat +datum=WGS84"))
  polyData <- data.frame(seq_len(length(polys)), row.names = names(polys))
  spdf <- SpatialPolygonsDataFrame(polys, data = polyData)
  
  plot(spdf, border = NA, bg = "gray90")
  
  if (nrow(tbl) > 0) {
    sp <- sp::SpatialPointsDataFrame(
      coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
      data = dplyr::select(tbl, -c(.data$lon, .data$lat))
    )
    goesaodc_plotSpatialPoints(sp, var = "AOD", cex = 0.5, breaks = breaks, 
                               add = TRUE)
  }
  maps::map("state", regions = states, add = TRUE)
  title(paste("GOES East AOD, DQF <=", opt$dqfLevel), cex.main = 3.5)
  
  # Legend color scale
  cols <- RColorBrewer::brewer.pal(length(breaks) - 1, "YlOrRd")
  col_i <- .bincode(seq(from = breaks[length(breaks)], to = breaks[1], by = -0.05), breaks)
  col_v <- cols[col_i]
  legendImage <- as.raster(matrix(col_v, ncol = 1))
  plot(0, 0, col = "transparent", xlim = c(-1, 1), ylim = c(breaks[1], breaks[length(breaks)]), 
       axes = FALSE, xlab = NA, ylab = NA, main = "AOD", cex.main = 3.0)
  axis(side = 2, line = -6, cex.axis = 2.0)
  rasterImage(legendImage, -0.3, breaks[1], 0.3, breaks[length(breaks)])
  
  # Timestamp clock plot
  hourFraction <- lubridate::hour(localHour) / 24
  pie(x = c(hourFraction, 1/24, 1 - (hourFraction + 1 / 24)), clockwise = TRUE,
      init.angle = 270, labels = NA, col = c("white", "black", "white"))
  text(0, -1.0, "Midnight", cex = 2.0)
  title(paste0(strftime(localHour, "%Y-%m-%d", tz = localTimezone), "\n", 
               strftime(localHour, "%H:%M", tz = localTimezone)), cex.main = 2.5)
  
  frameNumber <- frameNumber + 1
  dev.off()
}

# ----- Generate video ---------------------------------------------------------

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
