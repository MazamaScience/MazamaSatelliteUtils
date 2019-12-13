## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----satellite_setup, message=FALSE--------------------------------------
library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

## ----download_files------------------------------------------------------
datetime <- lubridate::ymd_h("2019-08-01 17", tz = "UTC")
downloadedFiles <- goesaodc_downloadAOD("G16", datetime)

## ----display_files-------------------------------------------------------
dateFiles <- goesaodc_listFiles("G16", datetime)
print(dateFiles)

## ----load_handle---------------------------------------------------------
nc <- goesaodc_openFile(filename = dateFiles[1])

## ----ncdf4_retrival------------------------------------------------------
readings_aod <- as.numeric(ncdf4::ncvar_get(nc, "AOD"))
print(length(readings_aod))
print(range(readings_aod, na.rm = TRUE))

## ----create_tibble-------------------------------------------------------
tb <- goesaodc_createTibble(nc)
knitr::kable(head(tb))

## ----spatialPlot_world, warning=FALSE------------------------------------
sp <- goesaodc_createSpatialPoints(nc, dqfLevel = 2)

maps::map("world")
goesaodc_plotSpatialPoints(sp, add = TRUE, cex = 0.1)

## ----spatialPlot_usa-----------------------------------------------------
maps::map(database = "state")
goesaodc_plotSpatialPoints(sp, add = TRUE, cex = 0.3)

## ----spatialPlot_high_quality--------------------------------------------
sp_dqf0 <- goesaodc_createSpatialPoints(nc, dqfLevel = 0)

maps::map("state")
goesaodc_plotSpatialPoints(sp_dqf0, cex = 0.3, add = TRUE)

## ----spatialPlot_oregon, message=FALSE-----------------------------------
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")

oregon <- subset(USCensusStates, stateCode == "OR")
bbox_oregon <- sp::bbox(oregon)
sp_oregon <- goesaodc_createSpatialPoints(nc, bbox = bbox_oregon, dqfLevel = 2)

lon_mp97 <- -123.268
lat_mp97 <- 42.913

plot(oregon)
goesaodc_plotSpatialPoints(sp_oregon, cex = 0.3, add = TRUE)
points(x = lon_mp97, y = lat_mp97, pch = 0, cex = 4.0, lwd = 1.5, 
       col = "darkgreen")

## ----create_raster_highdef-----------------------------------------------
rstr <- goesaodc_createRaster(nc, bbox = bbox_oregon, res = 0.1, dqfLevel = 2)
pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
pal_count <- colorRampPalette(c("powderblue", "dodgerblue4"))

raster::plot(rstr$AOD, col = pal_aod(50))
plot(oregon, add = TRUE)
points(x = lon_mp97, y = lat_mp97, pch = 0, cex = 4.0, lwd = 1.5, 
       col = "darkgreen")

## ----cell_area-----------------------------------------------------------
avgCellArea <- mean(raster::values(raster::area(rstr)))

## ------------------------------------------------------------------------
bbox_mp97 <- c(-124, -123, 42.5, 43.5)
pointCoords_mb97 <- dplyr::filter(tb, 
                                  lon > bbox_mp97[1], lon < bbox_mp97[2], 
                                  lat > bbox_mp97[3], lat < bbox_mp97[4])

rstrCount <- goesaodc_createRaster(nc, bbox = bbox_oregon, res = 0.1, 
                                   fun = "count", dqfLevel = 2)
raster::plot(rstrCount$AOD, main = "Cell Point Count", col = pal_count(50),
             xlim = bbox_mp97[1:2], ylim = bbox_mp97[3:4])
plot(oregon, add = TRUE)
points(pointCoords_mb97$lon, pointCoords_mb97$lat, pch = 16, cex = 0.3)
points(lon_mp97, lat_mp97, pch = 3, cex = 1, col = "red")

## ----create_rasterStack--------------------------------------------------
rstrStack <- goesaodc_createRasterStack(
  satID = "G16", 
  datetime = datetime,
  bbox = bbox_oregon, 
  dqfLevel = 3,
  res = 0.08)

## ----rasterStack_levelPlot, fig.width=8, fig.height=6--------------------
rasterVis::levelplot(rstrStack)

## ----rasterStack_average-------------------------------------------------
rstrStackAvg <- raster::mean(rstrStack, na.rm = TRUE)
raster::plot(rstrStackAvg, col = pal_aod(50))
plot(oregon, add = TRUE)
points(x = lon_mp97, y = lat_mp97, pch = 0, cex = 4.0, lwd = 1.5, 
       col = "darkgreen")

## ----timeseries_method, fig.width=11, fig.height=6-----------------------
datetimeLocal <- datetime
attributes(datetimeLocal)$tzone = "America/Los_Angeles"

ts_simple <- raster_createLocationTimeseries(rasterStack = rstrStack,
                                             longitude = lon_mp97, 
                                             latitude = lat_mp97, 
                                             bbox = bbox_oregon,
                                             method = "simple")

par(mfrow=c(1, 2))
raster::plot(rstrStackAvg, col = pal_aod(50), 
             xlim = c(-125, -122), ylim = c(42, 44),
             main = "Milepost 97 AOD", 
             xlab = "Longitude", ylab = "Latitude")
plot(oregon, add = TRUE)
points(x = lon_mp97, y = lat_mp97, pch = 3, lwd = 2)

plot(x = ts_simple$datetime, y = ts_simple$aod, pch = 16, cex = 1,
     main = paste(datetimeLocal, "PDT"), xlab = "Time (local)", ylab = "AOD")

## ----timeseries_buffer, fig.width=11, fig.height=6-----------------------
ts_buffer <- raster_createLocationTimeseries(rasterStack = rstrStack,
                                             longitude = lon_mp97,
                                             latitude = lat_mp97, 
                                             bbox = bbox_oregon,
                                             buffer = 20000,
                                             fun = mean)

par(mfrow=c(1, 2))
raster::plot(rstrStackAvg, col = pal_aod(50), 
             xlim = c(-125, -122), ylim = c(42, 44),
             main = "Milepost 97 AOD", 
             xlab = "Longitude", ylab = "Latitude")
plot(oregon, add = TRUE)
points(x = lon_mp97, y = lat_mp97, pch = 1, lwd = 0.5, cex = 5)
points(x = lon_mp97, y = lat_mp97, pch = 3, lwd = 2)

plot(x = ts_buffer$datetime, y = ts_buffer$aod, pch = 16, cex = 1,
     main = paste(datetimeLocal, "PDT"), xlab = "Time (local)", ylab = "AOD")

