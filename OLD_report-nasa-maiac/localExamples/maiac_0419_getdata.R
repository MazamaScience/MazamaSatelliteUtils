# Figure out what satellite data to download
library(PWFSLSmoke)
library(PWFSLSmokeModeling)
library(maps)

airnow <- airnow_load(2017, 10)
ca <- monitor_subset(airnow, stateCodes = "CA")
monitorPlot_timeseries(ca, style = "gnats")

# Load Oct 6 - 26 (20 days). start with Oct 9 (282)
file <- ncdf4::nc_open('~/Data/maiac/MAIACAAOT.h01v04.20172822150.nc')
latitude <- ncdf4::ncvar_get(file, 'grid1km_latitude')
longitude <- ncdf4::ncvar_get(file, 'grid1km_longitude')
aot <- ncdf4::ncvar_get(bop, 'Optical_Depth_055')
# maiac <- data_frame(latitude = as.numeric(latitude), longitude = as.numeric(longitude), aot = as.numeric(aot))

# map("state", "california")
# # points(lat, lon, pch = 17, cex = .2)
# # Convert swath to raster
# setModelDataDir("~/Data/Bluesky")
# source(paste0(baseDir, 'R/convertGridToRaster.R'))
# source(paste0(baseDir, 'R/convertRasterToGrid.R'))
# source(paste0(baseDir, 'R/convertSwathToRaster.R'))
# source(paste0(baseDir, 'R/loadRawMaiac.R'))
# bs <- bluesky_load(modelRun = 2017100900)
# bs_raster <- convertGridToRaster(bs)

# Projection information: http://spatialreference.org/ref/sr-org/modis-sinusoidal/
modis_crs <- CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs ")

aot_raster <- raster::raster(nrows = 1200, ncols = 1200, 
                             xmn = -2800000.000000, xmx = -1600000.000000,
                             ymn = 1200000.000000, ymx = 2400000.000000,
                             crs = CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "),
                             vals = t(aot))

aot_raster_latlong <- raster::raster(nrows = 1200, ncols = 1200, 
                             xmn = min(longitude), xmx=max(longitude),
                             ymn = min(latitude), ymx=max(latitude),
                             crs = CRS("+init=epsg:3310"))
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")

ca_modis <- spTransform(subset(USCensusStates, stateCode == "CA"), modis_crs)
plot(ca_modis)
plot(aot_raster)
plot(ca_modis, add = TRUE)

plot(USCensusStates, add = TRUE)
points(longitude, latitude, pch = 17, col = adjustcolor('black', alpha.f = .02), cex = .2)

# Don't do this for full grid size: it will crash R
mc_raster <- raster::rasterize(x = data_frame(as.numeric(longitude), latitude=as.numeric(latitude)), y = aot_raster_latlong, field = as.numeric(aot), fun = mean, na.rm=TRUE)
plot(mc_raster)
plot(USCensusStates, add = TRUE)
dim(bs)
