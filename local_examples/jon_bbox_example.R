library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")
loadSpatialData("USCensusCounties")

library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

files <- goesaodc_downloadAOD("G17", 2019102714, timezone = "America/Los_Angeles")
files <- goesaodc_listFiles("G17", 2019102714, timezone = "America/Los_Angeles")

nc <- goesaodc_openFile(basename(files[1]))

# Kincade fire
kincade_bbox <- c(-124, -120, 36, 39)

lonLo <- kincade_bbox[1]
lonHi <- kincade_bbox[2]
latLo <- kincade_bbox[3]
latHi <- kincade_bbox[4]

tbl <- goesaodc_createTibble(nc, kincade_bbox)

# ----- Code borrrowed from goesaodc_createSpatialPoints() ---------------------

spatialPoints <- sp::SpatialPointsDataFrame(
  coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
  data = dplyr::select(tbl, -c(.data$lon, .data$lat))
)

# use 10^AOD
spatialPoints@data$AOD <- 10^spatialPoints@data$AOD

# ----- The proof is in the pudding --------------------------------------------
maps::map('state', 'california', xlim = c(lonLo, lonHi), ylim = c(latLo, latHi))
usr <- par('usr')
graphics::rect(usr[1], usr[3], usr[2], usr[4], col = "gray80")
goesaodc_plotSpatialPoints(spatialPoints, add = TRUE,
                           cex = 0.5, n = 5e5,
                           breaks = c(0,5,10,20,40,Inf),
                           colBins = 5,
                           paletteName = "YlOrRd")
maps::map('county', 'california', col = "white", add = TRUE)
maps::map('state', 'california', lwd = 1.5, add = TRUE)
