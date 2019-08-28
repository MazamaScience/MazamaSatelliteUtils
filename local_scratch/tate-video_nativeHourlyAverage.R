#' Command line parameters
#' s = startdate
#' d = duration days / e = enddate 
#' r = region
#' q = data quality level (0, 1, 2)
#' v = verbose
#' V = version
#' 
#' Would be useful:
#' - Function that takes two dates, an input timezone and output timezone, and 
#' returns the sunrise time for the first day and the sunset time of the last in
#' the output timezone

library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)
library(magrittr)

setSatelliteDataDir("~/Data/Satellite/")
setSpatialDataDir("~/Data/Spatial")

startdate <- lubridate::ymd("2019-08-01", tz = "UTC")
duration <- lubridate::hours(2)

region <- "enc"
dqfLevel <- 0

# ----- Script start ---------------

if (region == "enc") {
  states <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin")
} else if (region == "pnw") {
  states <- c("Washington", "Oregon", "Idaho")
}

bbox <- maps::map("state", regions = states, fill = TRUE, plot = FALSE)$range

startHour <- startdate + lubridate::hours(17) # Sunrise on first day
endHour <- startHour + duration               # Sunset on last day

hours <- seq.POSIXt(from = startHour, to = endHour, by = "hour")
for (hour in as.list(hours)) {
  
  print(hour)
  hourString <- strftime(hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  #downloadedFiles <- goesaodc_downloadAOD(hourString)
  hourFiles <- goesaodc_listFiles(hourString)
  ncHandles <- purrr::map(hourFiles, goesaodc_openFile)
  
  # List of vectors of DQF level logicals for each scan
  dqfMasks <- 
    purrr::map(ncHandles, ncdf4::ncvar_get, varid = "DQF") %>% 
    purrr::map(as.vector) %>%
    purrr::map(function(x) { x > dqfLevel })
  
  # List of vectors of AOD values for each scan
  aodScans <- 
    purrr::map(ncHandles, ncdf4::ncvar_get, varid = "AOD") %>% 
    purrr::map(as.vector) 
  
  # Mask each AOD scan by its DQF mask
  for (i in 1:length(aodScans)) {
    aodScans[[i]][dqfMasks[[i]]] <- NA
  }
  
  # Average the remaining AOD readings of all the scans in the hour
  stackedAODScans <- do.call(rbind, aodScans)
  avgAODReadings <- colMeans(stackedAODScans, na.rm = FALSE)
  
  # Construct a tibble to hold projected lat/lon coords and AOD for the average scan matrix.
  varList <- list()
  varList[["AOD"]] <- avgAODReadings
  varList[["lon"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$longitude )
  varList[["lat"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$latitude )
  
  # Create a tibble with all columns but removing rows if any of the columns
  # are missing.
  tbl <-
    tibble::as_tibble(varList) %>%
    tidyr::drop_na()
  
  # Subset by bbox and DQF level
  tbl <- dplyr::filter(tbl, 
                       lon >= bbox[1], lon <= bbox[2],
                       lat >= bbox[3], lat <= bbox[4])
  
  # Draw frame
  maps::map("state", regions = states)
  title(paste(hourString, "UTC"))
  
  if (nrow(tbl) > 0) {
    # Create spatial points from tibble
    sp <- sp::SpatialPointsDataFrame(
      coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
      data = dplyr::select(tbl, -c(.data$lon, .data$lat))
    )
    
    # Plot spatial points
    goesaodc_plotSpatialPoints(sp, var = "AOD", 
                               cex = 0.25, breaks = c(-3.0, -0.2, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.5, 3.0), 
                               add = TRUE)
    maps::map("state", regions = states, add = TRUE)
  }
  
}
