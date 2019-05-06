# Tom's scratch file
# 5/2/2019

# ----- goesaodc_listFiles draft -----------------------------------------------

goesaodc_listFiles <- function(
  date,
  hour = NULL
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  date <- as.character(date)
  if (stringr::str_length(date) != 8) {
    stop(paste0("'date' must be of the format YYYYMMDD"))
  }
  
  if (!is.null(hour)) {
    hour <- as.character(hour)
    if (stringr::str_length(hour) > 2) {
      stop(paste0("'hour' must be of the format HH"))
    }
  }
  
  # ----- Get Matching Files ---------------------------------------------------
  
  dataDir <- getSatelliteDataDir()
  
  regex <- "OR_ABI-L2-AODC-M6_G16_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  dataFiles <- list.files(getSatelliteDataDir(), pattern = regex)
  startTimes <- purrr::map_chr(dataFiles, goesaodc_getStartString)
  
  # Convert date and hour to POSIXct, then to Julian date
  if ( is.null(hour) ) {
    startTime <- lubridate::parse_date_time(date, orders = "%Y%m%d")
    startTime <- format(startTime, "%Y%j")
  } else {
    startTime <- lubridate::parse_date_time(paste0(date, hour), 
                                            orders = "%Y%m%d%H")
    startTime <- format(startTime, "%Y%j%H")
  }
  
  # Find matching start times
  mask <- stringr::str_detect(startTimes, startTime)
  matchingFiles <- dataFiles[mask]
  
  return(matchingFiles)
}

# ----- goesaodc_listFiles and goesaodc_openFile example -----------------------

date <- 20190502
hour <- 12

# make sure files are downloaded
goesaodc_downloadAOD(date, hour)

nc <- goesaodc_listFiles(date, hour)[1] %>% goesaodc_openFile()

# create quick plot of SpatialPoints
goesaodc_createSpatialPoints(nc) %>%
  goesaodc_plotSpatialPoints()

maps::map("state", add=T)

# plot raster
raster <- goesaodc_createRaster(nc)
plot(raster$AOD)

maps::map("state", add=T)
