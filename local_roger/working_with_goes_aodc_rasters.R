## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)
setSatelliteDataDir("~/Data/Satellite")
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")

florida <- subset(USCensusStates, stateCode == "FL")
nc <- goesaodc_openFile("OR_ABI-L2-AODC-M6_G16_s20191291201274_e20191291204047_c20191291210009.nc")

pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
pal_dqf <- colorRampPalette(c("green", "red"))

## ------------------------------------------------------------------------
res <- 0.05
bbox <- sp::bbox(florida)

lon_min <- bbox[1, 1]
lon_max <- bbox[1, 2]
lat_min <- bbox[2, 1]
lat_max <- bbox[2, 2]

ncols <- ((lon_max - lon_min) / res) + 1
nrows <- ((lat_max - lat_min) / res) + 1

rasterLayer <- raster::raster(nrows = nrows, ncols = ncols,
                              xmn = lon_min, xmx = lon_max,
                              ymn = lat_min, ymx = lat_max,
                              crs = "+proj=longlat +datum=WGS84 +ellps=GRS80")

## ---- echo=FALSE---------------------------------------------------------
spatialPoints <- goesaodc_createSpatialPoints(nc = nc, bbox = bbox, dqfLevel = 2)
rasterBrick <- raster::rasterize(spatialPoints, rasterLayer, fun = mean)
plot(rasterBrick)

## ------------------------------------------------------------------------
spatialPoints

## ---- echo=FALSE, fig.width=8, fig.height=4------------------------------
par(mfrow=c(1, 2), mar=c(2, 4, 2, 4))

plot(rasterBrick$AOD, main = "AOD", col = pal_aod(50))
plot(florida, add = TRUE)

plot(rasterBrick$DQF, main = "DQF", col = pal_dqf(50))
plot(florida, add = TRUE)

## ------------------------------------------------------------------------
rb_simpler <- goesaodc_createRaster(nc = nc, 
                                    res = res, 
                                    fun = mean, 
                                    bbox = bbox, 
                                    dqfLevel = 2)

plot(rb_simpler$AOD, main = "AOD", col = pal_aod(50))
plot(florida, add = TRUE)

## ---- fig.width=8, fig.height=6------------------------------------------
rb_mean   <- raster::rasterize(spatialPoints, rasterLayer, fun = mean)
rb_median <- raster::rasterize(spatialPoints, rasterLayer, fun = median)
rb_sd     <- raster::rasterize(spatialPoints, rasterLayer, fun = sd)
rb_count  <- raster::rasterize(spatialPoints, rasterLayer, fun = "count")

par(mfrow=c(2, 2), mar=c(2, 3, 2, 3))
plot(rb_mean$AOD,   main = "AOD Mean", col = pal_aod(50))
plot(rb_median$AOD, main = "AOD Median", col = pal_aod(50))
plot(rb_sd$AOD,     main = "AOD Standard Deviation", col = pal_aod(50))
plot(rb_count$AOD,  main = "Points in Cells", col = pal_aod(50))

## ------------------------------------------------------------------------
rasterVis::levelplot(rasterBrick$AOD, main = "AOD", margin = FALSE)

