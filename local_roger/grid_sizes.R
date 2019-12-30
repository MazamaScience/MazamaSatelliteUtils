## ----setup, include=FALSE, message=FALSE---------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)

setSatelliteDataDir("~/Data/Satellite")
setSpatialDataDir("~/Data/Spatial")

# get bbox for Texas
loadSpatialData("USCensusStates")
tx <- subset(USCensusStates, stateCode == "TX")
bbox_tx <- sp::bbox(tx)

goesaodc_downloadAOD(satID = "G16", datetime = "20191291201274", isJulian = TRUE, timezone = "UTC")
nc <- goesaodc_openFile("OR_ABI-L2-AODC-M6_G16_s20191291201274_e20191291204047_c20191291210009.nc")
pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
pal_count <- colorRampPalette(c("powderblue", "dodgerblue4"))
pal_sd <- colorRampPalette(c("lightpink", "darkred"))

## ----plot_texas_points, fig.width=5, fig.height=5, fig.align='center', echo=FALSE----
tb <- goesaodc_createTibble(nc)
data_tx <- dplyr::filter(tb, 
                         lon > bbox_tx["x", "min"], lon < bbox_tx["x", "max"], 
                         lat > bbox_tx["y", "min"], lat < bbox_tx["y", "max"])

sp <- goesaodc_createSpatialPoints(nc, bbox = bbox_tx, dqfLevel = 2)

par(mar=c(0, 0, 2, 0))
plot(tx, main = "Texas AOD")
goesaodc_plotSpatialPoints(sp, var = "AOD", cex = 0.15, add = TRUE)
plot(tx, add = TRUE)

## ----texas_resolution_plots, fig.width=10, fig.height=4, fig.align='center', echo=FALSE----
par(mfrow=c(1, 3), mar=c(2, 1, 2, 1))
rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 0.1, dqfLevel = 2)
raster::plot(rstr$AOD, main = "AOD Res. 0.1", col = pal_aod(100), legend = FALSE)
plot(tx, add = TRUE)

rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 0.3, dqfLevel = 2)
raster::plot(rstr$AOD, main = "AOD Res. 0.3", col = pal_aod(100), legend = FALSE)
plot(tx, add = TRUE)

rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 1.0, dqfLevel = 2)
raster::plot(rstr$AOD, main = "AOD Res. 1.0", col = pal_aod(100), legend = FALSE)
plot(tx, add = TRUE)

## ----dense_plot, fig.width=8, fig.height=6, fig.align='center', echo=FALSE----
# Subset for the east coast of Texas where there is a lot of dense data
bb_tx_dense <- c(-97, -96, 27, 28)
tb_tx_dense <- dplyr::filter(tb, 
                             lon > bb_tx_dense[1], lon < bb_tx_dense[2], 
                             lat > bb_tx_dense[3], lat < bb_tx_dense[4])
rstr_mean <- goesaodc_createRaster(nc, 
                              bbox = bb_tx_dense, 
                              res = 0.1, fun = mean,
                              dqfLevel = 2)

# Plot the raster colored with the AOD palette and draw all sample point locations
raster::plot(rstr_mean$AOD, main = "AOD", col = pal_aod(25), xlim = bb_tx_dense[1:2], ylim = bb_tx_dense[3:4])
plot(tx, add = TRUE)
points(tb_tx_dense$lon, tb_tx_dense$lat, pch = 16, cex = 0.4)

## ----dense_average_plots, fig.width=10, fig.height=4, fig.align='center', echo=FALSE----
# Assign each reading its own color based on the AOD palette
tb_tx_dense$col <- pal_aod(25)[as.numeric(cut(tb_tx_dense$AOD, breaks = 25))]

# Generate the median raster
rstr_median <- goesaodc_createRaster(nc, 
                              bbox = bb_tx_dense, 
                              res = 0.1, 
                              fun = median,
                              dqfLevel = 2)

par(mfrow = c(1, 2), mar = c(2, 4.5, 2, 4.5))

# Plot the mean raster
raster::plot(rstr_mean$AOD, main = "Mean AOD", col = pal_aod(25), 
             xlim = bb_tx_dense[1:2], ylim = bb_tx_dense[3:4])
plot(tx, add = TRUE)
points(tb_tx_dense$lon, tb_tx_dense$lat, 
       pch = 16, cex = 0.5, col = tb_tx_dense$col)

# Plot the median raster
raster::plot(rstr_median$AOD, main = "Median AOD", col = pal_aod(25), 
             xlim = bb_tx_dense[1:2], ylim = bb_tx_dense[3:4])
plot(tx, add = TRUE)
points(tb_tx_dense$lon, tb_tx_dense$lat, 
       pch = 16, cex = 0.5, col = tb_tx_dense$col)

## ----dense_extra_plots, fig.width=10, fig.height=4, fig.align='center', echo=FALSE----
rstr_count <- goesaodc_createRaster(nc, 
                              bbox = bb_tx_dense, 
                              res = 0.1, 
                              fun = "count",
                              dqfLevel = 2)

rstr_sd <- goesaodc_createRaster(nc, 
                              bbox = bb_tx_dense, 
                              res = 0.1, 
                              fun = sd,
                              dqfLevel = 2)

par(mfrow = c(1, 2), mar = c(2, 4.5, 2, 4.5))
raster::plot(rstr_count$AOD, main = "Point Count", col = pal_count(25), 
             xlim = bb_tx_dense[1:2], ylim = bb_tx_dense[3:4])
plot(tx, add = TRUE)
points(tb_tx_dense$lon, tb_tx_dense$lat, pch = 16, cex = 0.3)


raster::plot(rstr_sd$AOD, main = "AOD Standard Deviation", col = pal_sd(25), 
             xlim = bb_tx_dense[1:2], ylim = bb_tx_dense[3:4])
plot(tx, add = TRUE, main = "AOD Standard Deviation")
points(tb_tx_dense$lon, tb_tx_dense$lat, pch = 16, cex = 0.3)

## ----texas_count_plots, fig.width=10, fig.height=4, fig.align='center', echo=FALSE----
par(mfrow=c(1, 3), mar=c(2, 1, 2, 1))
rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 0.1, fun = "count", dqfLevel = 2)
raster::plot(rstr$AOD, main = "Point Count Res: 0.1", col = pal_count(100), legend = FALSE)
plot(tx, add = TRUE)

rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 0.3, fun = "count", dqfLevel = 2)
raster::plot(rstr$AOD, main = "Point Count Res: 0.3", col = pal_count(100), legend = FALSE)
plot(tx, add = TRUE)
  
rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 1.0, fun = "count", dqfLevel = 2)
raster::plot(rstr$AOD, main = "Point Count Res: 1.0", col = pal_count(100), legend = FALSE)
plot(tx, add = TRUE)

## ----texas_sd_plots, fig.width=10, fig.height=4, fig.align='center', echo=FALSE----
par(mfrow=c(1, 3), mar=c(2, 1, 2, 1))
rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 0.1, fun = sd, dqfLevel = 2)
raster::plot(rstr$AOD, main = "AOD SD Res: 0.1", col = pal_sd(100), legend = FALSE)
plot(tx, add = TRUE)

rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 0.3, fun = sd, dqfLevel = 2)
raster::plot(rstr$AOD, main = "AOD SD Res: 0.3", col = pal_sd(100), legend = FALSE)
plot(tx, add = TRUE)
  
rstr <- goesaodc_createRaster(nc, bbox = bbox_tx, res = 1.0, fun = sd, dqfLevel = 2)
raster::plot(rstr$AOD, main = "AOD SD Res: 1.0", col = pal_sd(100), legend = FALSE)
plot(tx, add = TRUE)

