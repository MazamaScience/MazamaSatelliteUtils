# Library setup
library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)
setSatelliteDataDir("~/Data/Satellite/milepost97fire")
setSpatialDataDir("~/Data/Spatial")

# Get bbox for Oregon
loadSpatialData("USCensusStates")
or <- subset(USCensusStates, stateCode == "OR")
bbox_or <- sp::bbox(or)

# Download files for 3pm on July 31, 2019
# Pretty clear dark streak starting at Canyonville and extending south
date <- lubridate::ymd_h("2019-07-31 16", tz = "UTC")
paths <- goesaodc_downloadAOD(startdate = date)
files <- goesaodc_listFiles(date)

# Define AOD color palette
pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))

# Plot a raster for each file
for (i in c(1:length(files))) {
  nc <- goesaodc_openFile(filename = files[i])
  
  data <- goesaodc_createTibble(nc)
  data_or <- dplyr::filter(data,
                           lon > bbox_or["x", "min"], lon < bbox_or["x", "max"], 
                           lat > bbox_or["y", "min"], lat < bbox_or["y", "max"])
  
  rstr <- goesaodc_createRaster(nc, bbox = bbox_or, res = 0.08)
  
  if (TRUE) {
    # Fixed palette
    raster::plot(rstr$AOD, main = paste("Milepost 97 AOD", i), 
                 col = pal_aod(10), breaks = seq(-1.5, 2.5, by=0.4))
  } else {
    # Unfixed palette
    raster::plot(rstr$AOD, main = paste("Milepost 97 AOD", i))
  }
  
  plot(or, add = TRUE)
  points(x = c(-123.296895), y = c(42.858787), col = "red", cex = 3.0)
}
