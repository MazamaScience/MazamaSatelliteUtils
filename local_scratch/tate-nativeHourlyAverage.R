library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)
library(magrittr)

setSatelliteDataDir("~/Data/Satellite/")
setSpatialDataDir("~/Data/Spatial")

startHour <- lubridate::ymd("2019-08-01", tz = "UTC") + lubridate::hours(17)
endHour <- startHour + lubridate::hours(5)

hours <- seq.POSIXt(from = startHour, to = endHour, by = "hour")

for (hour in as.list(hours)) {
  
  print(hour)
  hourString <- strftime(hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  #downloadedFiles <- goesaodc_downloadAOD(hourString)
  hourFiles <- goesaodc_listFiles(hourString)
  
  # Average AOD of all the scans taken over the hour
  aodScans <- 
    purrr::map(hourFiles, goesaodc_openFile) %>%
    purrr::map(ncdf4::ncvar_get, varid = "AOD") %>%
    purrr::map(as.vector)
  stackedAODScans <- do.call(rbind, aodScans)
  avgAODVector <- colMeans(stackedAODScans, na.rm = FALSE)
  avgAODMatrix <- matrix(avgAODVector, nrow = 2500, ncol = 1500)
  
  #dqfScans <- 
  #  purrr::map(hourFiles, goesaodc_openFile) %>%
  #  purrr::map(ncdf4::ncvar_get, varid = "DQF") %>%
  #  purrr::map(as.vector)
  #stackedDQFScans <- do.call(rbind, aodScans)
  #avgAODVector <- colMeans(stackedAODScans, na.rm = FALSE) # ALWAYS
  #avgAODMatrix <- matrix(avgAODVector, nrow = 2500, ncol = 1500)
  
  # Mask this matrix with the DQF matrix (only values with DQF <= n are kept in
  # the avgAODMatrix, otherwise replaces with NA)
  
  # Construct a tibble to hold projected lat/lon coords and AOD for the average scan matrix.
  varList <- list()
  varList[["AOD"]] <- avgAODVector
  varList[["lon"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$longitude )
  varList[["lat"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$latitude )
  
  # TODO:  tidyr::drop_na() may be too restrictive if we have multiple data columns.
  
  # Create a tibble with all columns but removing rows if any of the columns
  # are missing.
  tbl <-
    tibble::as_tibble(varList) %>%
    tidyr::drop_na()
  
  sp <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
    data = dplyr::select(tbl, -c(.data$lon, .data$lat))
  )
  
  #image(avgAODMatrix[800:1200, 400:800])
  
  maps::map("state")
  goesaodc_plotSpatialPoints(sp, cex = 0.2, add = TRUE)
  maps::map("state", add = TRUE)
  title(hourString)
  
}






#' startdate, enddate/duration, region, DQF level
#' 
