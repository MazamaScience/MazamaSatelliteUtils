# createSandFiresData.R
# =====================
# This script will use the PWFSLSmoke package to create the following files associated with the worst week of the Sand fire.
# - Sand_monitors.RData -- ws_monitor object
# - Sand_bluesky.RData -- ws_grid object

#
# #' @export
# #' @title Read in a Raw MAIAC NetCDF file
# #' @param file absolute path of file to be converted
# #' @description Converts NASA MAIAC swath data from its origin format into a dataframe
# #' containing longitude, latitude, and aot.
# #' @return Dataframe with three columns.
# #' @examples
# #' \dontrun{
# #' placeholder
# #' }


# createSandFiresData <- function() {
 
# ----- Initial Setup -------------------

library(PWFSLSmoke)
library(MazamaSpatialUtils)
library(PWFSLSmokeModeling)
library(raster)

logger.setup()
setSpatialDataDir('~/Data/Spatial')
setModelDataDir('~/Data/Bluesky/')
loadSpatialData('NaturalEarthAdm1')

baseDir <- '~/Projects/Mazama/airfire-nasa-maiac/'

source(paste0(baseDir, 'R/convertGridToRaster.R'))
source(paste0(baseDir, 'R/convertRasterToGrid.R'))
source(paste0(baseDir, 'R/convertSwathToRaster.R'))
source(paste0(baseDir, 'R/loadRawMaiac.R'))

# ----- Monitor Data --------------------

# # Load 2016 AirNow Data
# epa_88101 <- epa_createMonitorObject(2016, parameterCode = 88101)
# save(epa_88101, file="localData/epa_88101.RData")
# epa_88502 <- epa_createMonitorObject(2016, parameterCode = 88502)
# save(epa_88502, file="localData/epa_88502.RData")
# 
# load("localData/epa_88101.RData")
# load("localData/epa_88502.RData")
# 
# # Load other data sources? (e.g. AIRSIS, WRCC?)
# 
# 
# # Combine and subset monitor data
# Sand_monitors <- monitor_combine(list(epa_88101, epa_88502))
# Sand_monitors <- monitor_subset(Sand_monitors, c(-125, -115), c(32, 37), c(20160721, 20160727))
# 
# save(Sand_monitors, file="localData/Sand_monitors.RData")

load(paste0(baseDir, 'localData/Sand_monitors.RData'))

# # ----- Bluesky Data --------------------
# 
# # SoCal is covered by the following three models:
# # - CANSAC-2km
# # - CANSAC-6km
# # - NAM-4km
# 
# # Load Bluesky run data
# Sand_bluesky_CANSAC_2km <- bluesky_aggregate(model = "CANSAC-2km", runStart = 2016072112, runEnd = 20160726)
# Sand_bluesky_CANSAC_6km <- bluesky_aggregate(model = "CANSAC-6km", runStart = 20160721, runEnd = 20160726)
# Sand_bluesky_NAM_4km <- bluesky_aggregate(model = "NAM-4km", runStart = 20160721, runEnd = 20160726, subDir = "forecast")
# 
# save(Sand_bluesky_CANSAC_2km, file="localData/Sand_bluesky_CANSAC_2km.RData")
# save(Sand_bluesky_CANSAC_6km, file="localData/Sand_bluesky_CANSAC_6km.RData")
# save(Sand_bluesky_NAM_4km, file="localData/Sand_bluesky_NAM_4km.RData")

load(paste0(baseDir, 'localData/Sand_bluesky_CANSAC_2km.RData'))
# load(paste0(baseDir, 'localData/Sand_bluesky_CANSAC_6km.RData'))
# load(paste0(baseDir, 'localData/Sand_bluesky_NAM_4km.RData'))

# # ----- MAIAC Data --------------------

maiac <- loadRawMaiac(paste0(baseDir, 'localData/maiac_sands_AQUA_20160721.nc'))

# # ----- Convert Bluesky & MAIAC data to rasters --------------------

ws_grid_2km <- Sand_bluesky_CANSAC_2km
ws_raster_2km <- convertGridToRaster(ws_grid_2km)
rm(Sand_bluesky_CANSAC_2km)

maiacRaster <- rasterize(x=maiac[c("longitude","latitude")], y=ws_raster_2km, field=maiac$aot)

# # ----- Crop data --------------

# crop bluesky and MAIAC 2km grid rasters to MAIAC swath bounding box
ext <- extent(c(range(maiac$longitude), range(maiac$latitude)))
ws_raster_2km <- crop(ws_raster_2km, ext)
maiacRaster <- crop(maiacRaster, ext)

# crop monitor data
Sand_monitors <- monitor_subset(Sand_monitors, xlim = ext[1:2], ylim = ext[3:4])

# # ----- Colors ----------------------

breaks_bs <- c(-10000,1,5,10,20,40,90,140,350,525,10000)
red_bs <- c(255,255,255,255,255,255,255,200,150)/255
green_bs <- c(225,195,165,135,105,75,46,2,3)/255
blue_bs <- c(225,195,165,135,105,75,45,3,3)/255
colors_bs <- c('transparent',grDevices::rgb(red=red_bs, green=green_bs, blue=blue_bs))

breaks_maiac <- seq(0, .225, length.out = 10)
colors_maiac <- RColorBrewer::brewer.pal(9,'YlOrRd')
colorIndex <- .bincode(maiac$aot, breaks_maiac, include.lowest=TRUE)

# # ----- Cleanup -----------------------

rm(list=c("red_bs", "green_bs", "blue_bs"))
   
# }
