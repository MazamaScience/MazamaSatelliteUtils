par(mar = c(3.1, 3.1, 4.1, 4.1))
plot(TERRA-AQUA, col = colors, breaks = breaks, colNA = "gray90", alpha = .8, main = paste0("Oct ", day), legend = FALSE)
plot(USCensusStates, add = TRUE)
usr <- par("usr")
legendbrks <- seq(usr[3], usr[4], length = length(breaks))
lx <- usr[2] + .5
rx <- usr[2] + .75
rect(lx,
     head(legendbrks, -1),
     rx,
     tail(legendbrks, -1),
     col = colors,
     xpd = NA)
axis(4, at = legendbrks, labels = c(paste0("-10^", rev(pow)), paste0("10^", pow)), pos = rx, las = 2)

plot(AQUA, breaks = breaks, col = colors, colNA = "gray90", main = "Oct 10: TERRA over AQUA")
plot(TERRA, breaks = breaks, col = colors, add = TRUE)
plot(USCensusStates, add =TRUE)




airnow <- airnow_load(2017)
wrcc <- wrcc_load(2017)
airsis <- airsis_load(2017)
mon <- monitor_combine(list(airnow, wrcc, airsis))
oct09 <- maiac_loadRaster("h01v04", 20171009, 2150, converterPath = "../executables/h4toncff_nc4") %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
ca <- monitor_subset(mon, tlim = c(20171008, 20171016), xlim = c(xmin(oct09), -114.5), ylim = c(ymin(oct09), 41))
plot(oct09)
plot(USCensusStates, add = TRUE)
sample <- c("lon_.122.941_lat_40.735_arb2.1020", "060130002_01",  "060658005_01",  "060110007_01", "lon_.118.778_lat_36.566_apcd.1030")
monitorSample <- monitor_subset(ca, monitorIDs = sample)
monitorMap(monitorSample, add = TRUE)


library(lubridate)
first <- ymd(20170501)
last <- ymd(20171101)
seqdays <- seq(first, last, '10 days')
junedays <- seq(ymd(20170501), ymd(20170510), 'days')
octdays <- seq(ymd(20171008), ymd(20171018), 'days')
days <- c(seqdays, junedays, octdays)

may01 <- maiac_loadRaster("h01v04", 20170501, 2105) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
may11 <- maiac_loadRaster("h01v04", 20170511, 2140) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
may21 <- maiac_loadRaster("h01v04", 20170521, 2040) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
may31 <- maiac_loadRaster("h01v04", 20170531) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
jun10 <- maiac_loadRaster("h01v04", 20170610, 2155) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
jun20 <- maiac_loadRaster("h01v04", 20170620) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
jun30 <- maiac_loadRaster("h01v04", 20170630, 2130) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
jul10 <- maiac_loadRaster("h01v04", 20170710, 2030) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
jul20 <- maiac_loadRaster("h01v04", 20170720, 2105) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
jul30 <- maiac_loadRaster("h01v04", 20170730, 2140) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
aug09 <- maiac_loadRaster("h01v04", 20170809, 2040) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
aug19 <- maiac_loadRaster("h01v04", 20170819) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
aug29 <- maiac_loadRaster("h01v04", 20170829, 2155) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
sep08 <- maiac_loadRaster("h01v04", 20170908) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
sep18 <- maiac_loadRaster("h01v04", 20170918, 2130) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
sep28 <- maiac_loadRaster("h01v04", 20170928, 2030) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
oct08 <- maiac_loadRaster("h01v04", 20171008, 2105) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
oct18 <- maiac_loadRaster("h01v04", 20171018, 2140) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
oct28 <- maiac_loadRaster("h01v04", 20171028, 2040) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
seqdata <- list(jun01, jun11, jun21, jun31, jul10, jul20, jul30, aug09, aug19, aug29, sep08, sep28, oct08, oct18, oct28)


oct01 <- maiac_loadRaster("h01v04", 20171001) %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
oct02 <- maiac_loadRaster("h01v04", 20171002, 2140) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
oct03 <- maiac_loadRaster("h01v04", 20171003, 2045) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
oct04 <- maiac_loadRaster("h01v04", 20171004, 2130) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))






oct08 <- maiac_loadRaster("h01v04", 20171008, 2105, converterPath = "../executables/h4toncff_nc4") %>% 
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
oct09 <- maiac_loadRaster("h01v04", 20171009, 2150, converterPath = "../executables/h4toncff_nc4") %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
oct10 <- maiac_loadRaster("h01v04", 20171010, converterPath = "../executables/h4toncff_nc4") %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
oct11 <- maiac_loadRaster("h01v04", 20171011, 2135, converterPath = "../executables/h4toncff_nc4") %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
oct12 <- maiac_loadRaster("h01v04", 20171012, 2040, converterPath = "../executables/h4toncff_nc4") %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
oct13 <- maiac_loadRaster("h01v04", ) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
oct14 <- maiac_loadRaster("h01v04", 20171014, 2030) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
oct15 <- maiac_loadRaster("h01v04", 20171015) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 

oct13 <- maiac_loadRaster("h01v04", 20171013) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 

jun01 <- maiac_loadRaster("h01v04", 20170601, product = "MAIACTAOT", 1845) %>%
   projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
jun02 <- maiac_loadRaster("h01v04", 20170602, 2105) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
jun03 <- maiac_loadRaster("h01v04", 20170603, 2150) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
jun04 <- maiac_loadRaster("h01v04", 20170604) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
jun05 <- maiac_loadRaster("h01v04", 20170605, 2135) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
jun06 <- maiac_loadRaster("h01v04", 20170606, 2040) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 
jun07 <- maiac_loadRaster("h01v04", 20170607) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) 



ca <- monitor_subset(mon, xlim = c(xmin(oct09), -114.5), ylim = c(ymin(oct09), 41))
ss <- monitor_subset(ca, tlim = c(20170501, 20171101))
ww <- monitor_subsetBy(ss, is.na(telemetryAggregator))
sampleids <- sample(ww$meta$monitorID, 5)
for (i in 1:5) { monitorPlot_timeseries(ww, monitorID = sampleids[i])}
sampleids <- c( "060374004_01", "060798002_01", "320031501_01", "060271003_01", "060110007_01")


octids <- c("060550003_01", "lon_.122.267_lat_38.220_arb2.1023", "lon_.122.133_lat_38.211_arb2.1034",
            "060950004_01","060131004_01", "060010013_01")


octsample <- monitor_subset(ca, monitorIDs = octids)
octsatellite <- tibble(date=as.POSIXct(octdays))
for(id in octids) {
  octsatellite[id] <- numeric()
}
for(i in 1:length(octdata)) {
  for (id in octids) {
    octsatellite[i, id] <- getValue(octdata[[i]], ca$meta[id, "longitude"], ca$meta[id, "latitude"])
  }  
}

octmonitor <- tibble(date=as.POSIXct(octdays))
for( id in octids ) {
  octmonitor[id] <- numeric()
  for(i in 1:length(octdata)) {
    octmonitor[i, id] <- octsample$data[[id]][which(octsample$data$datetime == octdays[i])]
  }
}


for(id in octids) {
  plot(octmonitor$date, octmonitor[[id]], type = "l", lty = 1, xlab = "date", ylab = "PM25", main = id)
  par(new = TRUE)
  plot(octsatellite$date, octsatellite[[id]], type = "l", lty = 2, axes=F, xlab=NA, ylab=NA)
  axis(side = 4)
  mtext(side = 4, line = 3, 'AOT')
  plot(octmonitor[[id]], octsatellite[[id]], main = id)
}


for(id in octids) {monitorPlot_timeseries(monitor_subset(octsample, monitorIDs = id, tlim = range(strftime(octdays, "%Y%m%d"))), main = id)}


monitorPlot_timeseries(monitor_subset(octsample, monitorIDs = "060010013_01" , tlim = range(strftime(octdays, "%Y%m%d"))), main = id)


tibble(id = octids, slope = NA, intercept = NA, r2 = NA, datapoints = NA)


colors <- colorRampPalette(c("white", "orange", "red"))(9)
breaks <- c(0, 0.03, 0.1, 0.2, 0.3, 0.5, 0.8, 10)
plot(USCensusStates, xlim = c(-142, -105), ylim = c(30.91, 44.91))
plot(oct08Afull, breaks = breaks, col = colors, colNA = "gray90", alpha = .8, main = "Full data extent", add = TRUE)
rect(-125.52, 36.52, -117.03, 41.78)
points(-122.2869, 38.2975)

crop(raster::extent(-125.52, -117.03, 36.52, 41.78))

oct08Afull <- maiac_loadRaster("h01v04", 20171008, 2105)
aeaStates <- spTransform(USCensusStates, CRS("+proj=aea +lat_1=20 +lat_2=60 +lon_0=-96 +lat_0=23 +datum=WGS84 +ellps=WGS84"))
napa <- SpatialPoints(data.frame(x = -122.2869, y = 38.2975), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
  spTransform(CRS("+proj=aea +lat_1=20 +lat_2=60 +lon_0=-96 +lat_0=23 +datum=WGS84 +ellps=WGS84")) 
crop <- SpatialPoints(data.frame(x = c(-125.52, -117.03, -117.03, -125.52, -125.52), y = c(36.52, 36.52, 41.78, 41.78, 36.52)), 
                        proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>%
  spTransform(CRS("+proj=aea +lat_1=20 +lat_2=60 +lon_0=-96 +lat_0=23 +datum=WGS84 +ellps=WGS84"))

plot(aeaStates, xlim = c(-3e+06, -1e+06), ylim = c(1e+06, 3e+06))
plot(oct08Afull, breaks = breaks, col = colors, colNA = "gray90", main = "Full data extent", add = TRUE, legend = FALSE)
plot(aeaStates, add = TRUE)
rect(xmin(oct08Afull), ymin(oct08Afull), xmax(oct08Afull), ymax(oct08Afull))
text(xmin(oct08Afull), ymax(oct08Afull), "tile extent", pos = 3)
polygon(coordinates(crop)[,1], coordinates(crop)[,2] )
text(coordinates(crop)[1,1], coordinates(crop)[1,2], "cropped \nextent", pos = 1)
plot(napa, add = TRUE)
text(xmin(napa), ymin(napa), "Napa", pos = 3)

oct09 <- oct09Tpre <- maiac_loadRaster("h01v04", 20171009, product = "MAIACTAOT")
smooth <- focal(oct09, w=matrix(1,3,3), fun=mean, na.rm = TRUE) 
oct09smoothproj <- projectRaster(smooth, crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>%
  crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
oct09projsmooth <- projectRaster(oct09, crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
  crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) %>%
  focal(w=matrix(1,3,3), fun=mean, na.rm = TRUE)
oct09proj <- projectRaster(oct09, crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
  crop(raster::extent(-125.52, -117.03, 36.52, 41.78))


lmaqua <- lm(dfa$monitor~dfa$satellite)
lmterra <- lm(dft$monitor~dft$satellite)

plot(dfa$satellite, dfa$monitor, main = "Oct 08-13 21:00 UTC (AQUA)", xlab = "monitoring data (PM2.5 ug/m3)", ylab = "satellite data (AOT)", xlim = c(0,2), ylim = c(0,300))
abline(lmaqua)
text(.35, 250, paste0("y = ", round(lmaqua$coefficients[1], 2), " + ", round(lmaqua$coefficients[2], 2), "x"), pos = 3)
text(.35, 250, paste0("adjusted r-squared = ", round(summary(lmaqua)$adj.r.squared, 2)), pos = 1)
plot(dft$satellite, dft$monitor, main = "Oct 08-13 19:00 UTC (TERRA)", xlab = "monitoring data (PM2.5 ug/m3)", ylab = "satellite data (AOT)", xlim = c(0,2), ylim = c(0,300))
abline(lmterra)
text(.35, 250, paste0("y = ", round(lmterra$coefficients[1], 2), " + ", round(lmterra$coefficients[2], 2), "x"), pos = 3)
text(.35, 250, paste0("adjusted r-squared = ", round(summary(lmterra)$adj.r.squared, 2)), pos = 1)


dfall <- add_row(dft, monitorID = dfa$monitorID, satellite = dfa$satellite, monitor = dfa$monitor)
plot(dfall$satellite, dfall$monitor)
lmall <- lm(dfall$monitor~dfall$satellite)
abline(lmall)
summary(lmall)


# ROC analysis for each AQI threshold
skill_ROCPlot(dfall$satellite, dfall$monitor, t1Range = c(0, 2), t2s = AQI$breaks_24[2:6], colors = AQI$colors)
skill_ROC(dfall$satellite, dfall$monitor, t1Range = c(0,2), t2 = 12)
skill_ROC(dfall$satellite, dfall$monitor, t1Range = c(0,2), t2 = 35.5)
skill_ROC(dfall$satellite, dfall$monitor, t1Range = c(0,2), t2 = 55.5)
skill_ROC(dfall$satellite, dfall$monitor, t1Range = c(0,2), t2 = 150.5)
skill_ROC(dfall$satellite, dfall$monitor, t1Range = c(0,2), t2 = 250.5)



maiacaqi <- c(0.1, 0.26, 0.3, .46, .48)

############------------------BLUESKY-----------------------#############

library(PWFSLSmokeModeling)


setModelDataDir("~/Data/Bluesky")
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")
#bs <- bluesky_aggregate("CANSAC-2km", 2017100800, 2017101523)
bs_full <- bluesky_aggregate("CANSAC-2km", 2017100800, 2017101523, subDir = "forecast")
bsoct1321 <- convertGridToRaster(bs_full, time = "2017101321") %>%
  crop(raster::extent(-125.52, -117.03, 36.52, 41.78))

colors <- colorRampPalette(c("white", "orange", "red"))(9)
bsbreaks <- c(0, 1, 20, 40, 80, 160, 320, 3500)
maiacbreaks <- c(0, 0.1, 0.2, 0.4, 0.8, 1.6, 3.2, 10)
oct13A <- maiac_loadRaster("h01v04", 20171013) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
  focal(w=matrix(1,3,3), fun=mean, na.rm = TRUE) %>%
  crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
plot(bsoct1321, breaks = bsbreaks, col = colors, colNA = "gray90", legend = FALSE)
plot(USCensusStates, add = TRUE)
plot(oct13A, breaks = maiacbreaks, col = colors, colNA = "gray90", legend = FALSE)
plot(USCensusStates, add = TRUE)

## Make normalized versions of both for plotting
bs_normal <- bsoct1321
bs_normal_values <- exp(scale(log(values(bsoct1321)[which(values(bsoct1321) != 0)])))
bs_values <- values(bs_normal)
bs_values[which(values(bs_normal) !=0 )] <- bs_normal_values
values(bs_normal) <- bs_values
range(values(bs_normal))

maiac_normal <- oct13A
maiac_normal_values <- exp(scale(log(values(oct13A)[which(values(oct13A) > .1)])))
maiac_values <- values(maiac_normal)
maiac_values[which(values(maiac_normal) > .1 )] <- maiac_normal_values
values(maiac_normal) <- maiac_values
range(values(maiac_normal), na.rm = TRUE)


breaks <- c(0, exp(c(-2, -1, 0, 1, 2)), 150)

plot(bs_normal, breaks = breaks, col = colors)
plot(USCensusStates, add = TRUE)

plot(maiac_normal, breaks = breaks, col = colors)
plot(USCensusStates, add = TRUE)


## resample maiac to match bluesky
maiac_matching <- resample(oct13A, bsoct1321)
maiac_land <- mask(maiac_matching, subset(USCensusStates, stateCode %in% c("CA", "NV")))
bs_land <- mask(bsoct1321, subset(USCensusStates, stateCode %in% c("CA", "NV")))
bs_smoothed <- focal(bs_land$pm25_100_2017101321, w=matrix(1,3,3), fun=mean, na.rm = TRUE)
plot(maiac_land, breaks = maiacbreaks, col = colors)
plot(bs_land, breaks = bsbreaks, col = colors)
plot(bs_smoothed, breaks = bsbreaks, col = colors)

bs10k <- aggregate(bs_land, 5)
#camask <- mask(bsoct1321, USCensusStates)

maiac10k <- aggregate(maiac_land, 5)

plot(bs10k, breaks = bsbreaks, col = colors)
plot(USCensusStates, add = TRUE)
plot(maiac10k, breaks = maiacbreaks, col = colors)
plot(USCensusStates, add = TRUE)

plot(values(bs_smoothed)~values(maiac_land), pch = 18, ylab = "Bluesky", xlab = "MAIAC AOT")

plot(values(bs10k)~values(maiac10k), pch = 18, ylab = "Bluesky", xlab = "MAIAC AOT",  col = adjustcolor(1, alpha.f = .2))

plot(bs_smoothed, breaks = bsbreaks, col = colors)
plot(USCensusStates, add =TRUE
)
monitor_subset(fireMonitors, tlim = c(2017101319, 2017101320)) %>%
monitorMap(add = TRUE)


monitorCount <- length(fireMonitors$meta$monitorID)

df <- data_frame(monitorID = fireMonitors$meta$monitorID, 
                 monitor = vector("numeric", monitorCount),
                 bluesky = vector("numeric", monitorCount),
                 satellite = vector("numeric", monitorCount))

# Get the the index for the correct day and time
timeIndex <- which(fireMonitors$data$datetime == lubridate::ymd_h(paste0("2017101321")) )

# Add monitor and satellite data for each monitor
for (i in 1:monitorCount) {
  monitorInfo <- fireMonitors$meta[i,]
  df[i,'monitor'] <- fireMonitors$data[timeIndex, monitorInfo$monitorID]
  df[i, 'bluesky'] <- getValue(bs_smoothed, monitorInfo$longitude, monitorInfo$latitude)
  df[i, 'satellite'] <- getValue(oct13A, monitorInfo$longitude, monitorInfo$latitude)
}

both <- lm(df$monitor~df$bluesky+df$satellite)
sat <- lm(df$monitor~df$satellite)
bs <- lm(df$monitor~df$bluesky)
bsmaiac <- lm(values(bs_smoothed)~values(maiac_land))

values_sat2bs <- values(maiac_land)
values_sat2bs[which(!is.na(values_sat2bs) & !is.na(values(bs_smoothed)))] <- bsmaiac$fitted.values
sat <- maiac_land
values(sat) <- values_sat2bs
plot(sat, breaks = bsbreaks, col = colors)
plot(USCensusStates, add = TRUE)
plot(bs_smoothed, breaks = bsbreaks, col = colors)
plot(USCensusStates, add = TRUE)

hist(values(sat-bs_smoothed), 1000, xlim = c(-20, 20))

difcolors <- colorRampPalette(c("blue", "white", "red"))(9)
difbreaks <- c(-2000, -100, -50, -20, -10, 10, 20, 50, 100, 2000)
plot(sat-bs_smoothed, breaks = difbreaks, col = difcolors)
plot(USCensusStates, add = TRUE)


bsoct1321 <- convertGridToRaster(bs_full, time = "2017101321") %>%
  crop(raster::extent(-125.52, -117.03, 36.52, 41.78))






png("~/desktop/context_map.png", 400, 500)
par(mar = c(0,0,0,0))
colors <- colorRampPalette(c("white", "orange", "red"))(9)
breaks <- c(0, 0.03, 0.1, 0.2, 0.3, 0.5, 0.8, 10)
aeaStates <- spTransform(USCensusStates, CRS("+proj=aea +lat_1=20 +lat_2=60 +lon_0=-96 +lat_0=23 +datum=WGS84 +ellps=WGS84"))
napa <- SpatialPoints(data.frame(x = -122.2869, y = 38.2975), proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
  spTransform(CRS("+proj=aea +lat_1=20 +lat_2=60 +lon_0=-96 +lat_0=23 +datum=WGS84 +ellps=WGS84")) 
crop <- SpatialPoints(data.frame(x = c(-125.52, -117.03, -117.03, -125.52, -125.52), y = c(36.52, 36.52, 41.78, 41.78, 36.52)), 
                      proj4string = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>%
  spTransform(CRS("+proj=aea +lat_1=20 +lat_2=60 +lon_0=-96 +lat_0=23 +datum=WGS84 +ellps=WGS84"))

plot(aeaStates, xlim = c(-2.9e+06, -1e+06), ylim = c(1e+06, 3e+06))
plot(oct08Afull, breaks = breaks, col = colors, colNA = "gray90", main = "Full data extent", add = TRUE, legend = FALSE)
plot(aeaStates, add = TRUE)
rect(xmin(oct08Afull), ymin(oct08Afull), xmax(oct08Afull), ymax(oct08Afull))
text(xmin(oct08Afull), ymax(oct08Afull), "tile extent", pos = 3)
polygon(coordinates(crop)[,1], coordinates(crop)[,2] )
text(coordinates(crop)[1,1], coordinates(crop)[1,2], "cropped \nextent", pos = 1)
plot(napa, add = TRUE)
text(xmin(napa), ymin(napa), "Napa", pos = 3)
dev.off()


oct09Ap <- maiac_loadRaster("h01v04", 20171009, 2150) %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
oct09Tp <- maiac_loadRaster("h01v04", 20171009, product = "MAIACTAOT") %>%
  projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))



png("~/desktop/fullextent_aqua.png", 1500, 1500)
par(mar = c(0,0,0,0))
plot(oct09Ap, legend = FALSE, breaks = breaks, col = colors, colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/fullextent_terra.png", 1500, 1500)
par(mar = c(0,0,0,0))
plot(oct09Tp, legend = FALSE, breaks = breaks, col = colors, colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()


png("~/desktop/differencemap.png", 1500, 1500)
colors <- colorRampPalette(c("blue", "white", "red"))(9)
breaks <- c(-5, -1, -.5, -.25, -.1, .1, .25, .5, 1, 5)
par(mar = c(0,0,0,4))
  AQUA <- oct09Ap
  TERRA <- oct09Tp
  plot(AQUA-TERRA, col = colors, breaks = breaks, colNA = "gray90", alpha = .8, legend = FALSE)
  plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/legend.png", 80, 500)
par(mar = c(1,1,1,3))
plot(0,0, col = "transparent", axes = FALSE, ann = F)
usr <- par("usr")
legendbrks <- seq(usr[3], usr[4], length = length(breaks))
lx <- usr[1]
rx <- usr[2] 
rect(lx,
     head(legendbrks, -1),
     rx,
     tail(legendbrks, -1),
     col = colors,
     xpd = NA)
axis(4, at = legendbrks, labels = breaks, pos = rx, las = 2)
dev.off()


png("~/desktop/oct12_monitormap.png", 700, 550)

par(mar = c(0,0,0,0))
# automatically-generated color palatte
colors <- colorRampPalette(c("white", "orange", "red"))(9)

# define breaks for colors
breaks <- c(0, 0.03, 0.1, 0.2, 0.3, 0.5, 0.8, 3)

  
  # Do the same for AQUA
  plot(oct12A, breaks = breaks, col = colors, colNA = "gray90", alpha = .8, legend = FALSE)
  plot(USCensusStates, add = TRUE)
  monitor_subset(fireMonitors, tlim = c(2017101221, 2017101222) ) %>% 
    monitorMap(add = TRUE, cex = 1.5)
dev.off()

plot(oct12A, breaks = c(0,thresholds, 3), col = adjustcolor(AQI$colors, alpha.f = .3), colNA = "gray90")
plot(USCensusStates, add = TRUE)
monitor_subset(fireMonitors, tlim = c(2017101221, 2017101222) ) %>% 
  monitorMap(add = TRUE)

png("~/desktop/oct12_thresholdmap.png", 700, 550)

par(mar = c(0,0,0,0))
# automatically-generated color palatte


# Do the same for AQUA
plot(oct12A, breaks = c(0, thresholds, 3), col = adjustcolor(AQI$colors, alpha.f = .3), colNA = "gray90", legend = FALSE)
plot(USCensusStates, add = TRUE)
monitor_subset(fireMonitors, tlim = c(2017101221, 2017101222) ) %>% 
  monitorMap(add = TRUE, cex = 1.5)
dev.off()



png("~/desktop/scatter_mon.png", 600, 450)
par(mar = c(4.2, 4.2, 2, 2))
plot(dfall$satellite, dfall$monitor, ylab = "monitoring data (PM2.5 ug/m3)", xlab = "satellite data (AOD)", cex.lab = 1.2, cex.axis = 1.2, cex = 1.2)
dev.off()

png("~/desktop/linear_mon.png", 600, 450)
par(mar = c(4.2, 4.2, 2, 2))
plot(dfall$satellite, dfall$monitor, ylab = "monitoring data (PM2.5 ug/m3)", xlab = "satellite data (AOD)", cex.lab = 1.2, cex.axis = 1.2, cex = 1.2)
abline(lm(dfall$monitor~dfall$satellite))
dev.off()

png("~/desktop/thresholds_mon.png", 600, 450)
par(mar = c(4.2, 4.2, 2, 2))
plot(dfall$satellite, dfall$monitor, ylab = "monitoring data (PM2.5 ug/m3)", xlab = "satellite data (AOD)", cex.lab = 1.2, cex.axis = 1.2, cex = 1.2)
abline(h = AQI$breaks_24[-1], col = adjustcolor(AQI$colors[-1], alpha.f = .6), lwd = 3)
dev.off()

png("~/desktop/thresholds_full_mon.png", 600, 450)
par(mar = c(4.2, 4.2, 2, 2))
plot(dfall$satellite, dfall$monitor, ylab = "monitoring data (PM2.5 ug/m3)", xlab = "satellite data (AOD)", cex.lab = 1.2, cex.axis = 1.2, cex = 1.2)
abline(lm(dfall$monitor~dfall$satellite), lty = 2, col = adjustcolor(1, alpha.f = .8))
points(AQI$breaks_24[2:6]~ thresholds, col = AQI$colors[-1], pch = 16, cex = 2)
abline(h = AQI$breaks_24[-1], col = adjustcolor(AQI$colors[-1], alpha.f = .6), lwd = 3)
abline(v = thresholds, col = adjustcolor(AQI$colors[-1], alpha.f = .6), lwd = 3)
dev.off()

abline(lm(dfall$monitor~dfall$satellite), lty = 2, col = adjustcolor(1, alpha.f = .8))
points(AQI$breaks_24[2:6]~ thresholds, col = AQI$colors[-1], pch = 16)
abline(h = AQI$breaks_24[-1], col = adjustcolor(AQI$colors[-1], alpha.f = .6), lwd = 2)
abline(v = thresholds, col = adjustcolor(AQI$colors[-1], alpha.f = .6), lwd = 2)




colors <- colorRampPalette(c("white", "orange", "red"))(9)
bsbreaks <- c(0, 1, 20, 40, 80, 160, 320, 3500)
maiacbreaks <- c(0, 0.1, 0.2, 0.4, 0.8, 1.6, 3.2, 10)

png("~/desktop/maiac_land.png", 500,400)
par(mar = c(0,0,0,0))
plot(maiac_land, breaks = c(0,thresholds, 3), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/bsmap.png", 500, 400)
par(mar = c(0,0,0,0))
plot(bs_mean, breaks = c(0, AQI$breaks_24[2:6], 3000), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/maiac_land_big.png", 1000,800)
par(mar = c(0,0,0,0))
plot(maiac_land, breaks = c(0,thresholds, 3), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/bsmap_big.png", 1000, 800)
par(mar = c(0,0,0,0))
plot(bs_mean, breaks = c(0, AQI$breaks_24[2:6], 3000), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/bsmap_presmooth_big.png", 1000, 800)
par(mar = c(0,0,0,0))
plot(bsoct1021, breaks = c(0, AQI$breaks_24[2:6], 3000), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90", ylim = c(0,1200))
plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/bsmap_nospatial_big.png", 1000, 800)
par(mar = c(0,0,0,0))
plot(bs_mean_1, breaks = c(0, AQI$breaks_24[2:6], 3000), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/bs_scatter_nospatial.png", 600, 450)
par(mar = c(4.2, 4.2, 2, 2))
plot(values(maiac_matching), values(bs_mean_1), xlab = "Satellite data (AOD)", ylab = "Bluesky (PM2.5)", ylim = c(0, 1200))
abline(lm(dfall$monitor~dfall$satellite), col = adjustcolor(1, alpha.f = .8))
dev.off()

png("~/desktop/bsmap_focal_big.png", 1000, 800)
par(mar = c(0,0,0,0))
plot(bs_focal, breaks = c(0, AQI$breaks_24[2:6], 3000), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()

png("~/desktop/bs_scatter_focal.png", 600, 450)
par(mar = c(4.2, 4.2, 2, 2))
plot(values(maiac_matching), values(bs_focal), xlab = "Satellite data (AOD)", ylab = "Bluesky (PM2.5)", ylim = c(0,1200))
abline(lm(dfall$monitor~dfall$satellite), col = adjustcolor(1, alpha.f = .8))
dev.off()

png("~/desktop/bs_scatter_presmooth.png", 600, 450)
par(mar = c(4.2, 4.2, 2, 2))
plot(values(maiac_matching), values(bsoct1021), xlab = "Satellite data (AOD)", ylab = "Bluesky (PM2.5)")
abline(lm(dfall$monitor~dfall$satellite), col = adjustcolor(1, alpha.f = .8))
dev.off()


png("~/desktop/bs_scatter.png", 600, 450)
par(mar = c(4.2, 4.2, 2, 2))
plot(values(maiac_matching), values(bs_mean), xlab = "Satellite data (AOD)", ylab = "Bluesky (PM2.5)", ylim= c(0, 1200))
abline(lm(dfall$monitor~dfall$satellite), col = adjustcolor(1, alpha.f = .8))
dev.off()


plot(maiac_land, breaks = c(0,thresholds, 3), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90")
plot(USCensusStates, add = TRUE)
monitor_subset(fireMonitors, tlim = c(2017101321, 2017101322) ) %>% 
  monitorMap(add = TRUE)

plot(bs_mean, breaks = c(0, AQI$breaks_24[2:6], 3000), col = adjustcolor(AQI$colors, alpha.f = .5), colNA = "gray90")
plot(USCensusStates, add = TRUE)
monitor_subset(fireMonitors, tlim = c(2017101321, 2017101322) ) %>% 
  monitorMap(add = TRUE)

lm1 <- lm(monitor~satellite, data = dfall)
newdata <- data.frame(satellite = values(maiac_matching))
maiac_matching_predicted <- maiac_matching
values(maiac_matching_predicted) <- predict(lm1, newdata)

difcolors <- colorRampPalette(c("blue", "white", "red"))(9)
difbreaks <- c(-2000, -100, -50, -20, -10, 10, 20, 50, 100, 2000)
plot(bs_mean-maiac_matching_predicted, breaks = difbreaks, col = difcolors, legend = FALSE, main = "Difference from MAIAC (predicted PM2.5) to Bluesky")
plot(USCensusStates, add = TRUE)

png("~/desktop/bsdiffmap_big.png", 1000, 800)
par(mar = c(0,0,0,0))
plot(bs_mean-maiac_matching_predicted, breaks = difbreaks, col = difcolors, legend = FALSE, colNA = "gray90")
plot(USCensusStates, add = TRUE)
dev.off()

values(baseline_matching) <- predict(lm1, data.frame(satellite = values(baseline_matching)))
bs_with_baseline <- baseline_matching + bs_mean
plot(bs_with_baseline-maiac_matching_predicted, breaks = difbreaks, col = difcolors, legend = FALSE)
#plot(bs_mean - maiac_matching_predicted, breaks = difbreaks, col = difcolors, legend = FALSE)
plot(values(maiac_matching), values(bs_with_baseline), xlab = "Satellite data (AOD)", ylab = "Bluesky (PM2.5)")
plot(bs_mean - maiac_matching_predicted, breaks = difbreaks, col = difcolors, legend = FALSE)
plot(USCensusStates, add = TRUE)


png("~/desktop/difpm25legend.png", 80, 500)
par(mar = c(1,1,1,3))
plot(0,0, col = "transparent", axes = FALSE, ann = F)
usr <- par("usr")
legendbrks <- seq(usr[3], usr[4], length = length(difbreaks))
lx <- usr[1]
rx <- usr[2] 
rect(lx,
     head(legendbrks, -1),
     rx,
     tail(legendbrks, -1),
     col = difcolors,
     xpd = NA)
axis(4, at = legendbrks, labels = difbreaks, pos = rx, las = 2)
dev.off()



plot(rumpled)
plot(land)



toAOD <- function(pm25) {
  bext <- 2.5 * 10^(-6) * pm25
  aod <- bext * 10000
  return(aod)
}





# Explore effects of bluesky events
events <- bluesky_loadEvents(startdate = 20170901, enddate = 20170909,
                             model = "PNW-4km", baseUrl = "https://haze.airfire.org/bluesky-daily/output")
chetcoevents <- event_subset(events, xlim = c(-125, -118), ylim = c(39, 45))




# Explore linear model output visually
library(dplyr)
library(tidyr)
library(purrr)

by_mon <- dfall %>% group_by(monitorID) %>% nest()

aod_model <- function(df) {
  result <- try({
    lm1 <- lm(monitor~satellite, data = df)
    }, silent = TRUE)
  if ("try-error" %in% class(result)) {
    return(NA)
  } else {
    return(lm1) 
  }
}



models <- by_mon %>% 
  mutate(mod = purrr::map(data, aod_model)) %>%
  mutate(meanAOD = unlist(purrr::map(data, function(df) { mean(df$satellite, na.rm = TRUE) }))) %>%
  mutate(meanPM = unlist(purrr::map(data, function(df) { mean(df$monitor, na.rm = TRUE) } ))) %>%
  mutate(int = unlist(purrr::map(mod, function(mod) {if(!is.na(mod)[1]){mod$coefficients[1] } else {NA}  }))) %>%
  mutate(slope = unlist(purrr::map(mod, function(mod) { if(!is.na(mod)[1]){ mod$coefficients[2] } else {NA} }))) %>%
  mutate(r2 = unlist(purrr::map(mod, function(mod) {if (!is.na(mod)[1]){ summary(mod)$r.squared } else {NA} }))) %>%
  mutate(n = unlist(purrr::map(data, function(df) { sum(!is.na(df$monitor) & !is.na(df$satellite)) }))) %>% 
  mutate(longitude = unlist(purrr::map(data, function(df){ df$longitude[1] }))) %>%
  mutate(latitude = unlist(purrr::map(data, function(df){ df$latitude[1] }))) %>%
  filter(n >=6)

# Map slope
breaks <- c(-Inf, -50, 0, 50, 100, 150, 200, Inf)
index <- .bincode(models$slope, breaks = breaks)
color <- colorRampPalette(c("yellow", "red"))(7)
plot(USCensusStates, xlim = c(-125.52, -117.03), ylim = c(36.52, 41.78))
points(models$longitude, models$latitude, col = color[index], pch = 16)

# Manually add a legend to the right side
usr <- par("usr")
legendbrks <- seq(usr[3], usr[4], length = length(breaks))
lx <- usr[2] + .6
rx <- usr[2] + .75
rect(lx,
     head(legendbrks, -1),
     rx,
     tail(legendbrks, -1),
     col = color,
     xpd = NA)
axis(4, at = legendbrks, labels = breaks, pos = rx, las = 2)
mtext("Slope", 4, line = 0.2)

# Map Int
breaks <- c(-Inf, -10, 0, 5, 10, 20, 30, Inf)
index <- .bincode(models$int, breaks = breaks)
color <- colorRampPalette(c("yellow", "red"))(7)
plot(USCensusStates, xlim = c(-125.52, -117.03), ylim = c(36.52, 41.78))
points(models$longitude, models$latitude, col = color[index], pch = 16)

# Manually add a legend to the right side
usr <- par("usr")
legendbrks <- seq(usr[3], usr[4], length = length(breaks))
lx <- usr[2] + .6
rx <- usr[2] + .75
rect(lx,
     head(legendbrks, -1),
     rx,
     tail(legendbrks, -1),
     col = color,
     xpd = NA)
axis(4, at = legendbrks, labels = breaks, pos = rx, las = 2)
mtext("Slope", 4, line = 0.2)

# Map r2
breaks <- c(0, .2, .4, .6, .8, 1)
index <- .bincode(models$r2, breaks = breaks)
color <- colorRampPalette(c("yellow", "red"))(5)
plot(USCensusStates, xlim = c(-125.52, -117.03), ylim = c(36.52, 41.78))
points(models$longitude, models$latitude, col = color[index], pch = 16)

# Manually add a legend to the right side
usr <- par("usr")
legendbrks <- seq(usr[3], usr[4], length = length(breaks))
lx <- usr[2] + .6
rx <- usr[2] + .75
rect(lx,
     head(legendbrks, -1),
     rx,
     tail(legendbrks, -1),
     col = color,
     xpd = NA)
axis(4, at = legendbrks, labels = breaks, pos = rx, las = 2)
mtext("R2", 4, line = 0.2)
