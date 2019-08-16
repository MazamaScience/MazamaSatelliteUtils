library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)

setSatelliteDataDir("~/Data/Satellite")
setSpatialDataDir("~/Data/Spatial")

loadSpatialData("USCensusStates")

# Define the region of interest (Milepost 97 Fire in Oregon)
oregon <- subset(USCensusStates, stateCode == "OR")
bbox_oregon <- sp::bbox(oregon)
lon <- -123.245
lat <-   42.861
#lon <- -123.268
#lat <- 42.913

# User must provide both the desired date AND the timezone for the raster region
dateLocal <- lubridate::ymd("2019-08-01", tz = "America/Los_Angeles")

# Convert the local date to a UTC date
dateUTC <- dateLocal
attributes(dateUTC)$tzone <- "UTC"

# Then gather local timeinfo from that UTC date
dateInfo <- PWFSLSmoke::timeInfo(dateUTC, 
                                 longitude = lon, latitude = lat, 
                                 timezone = "America/Los_Angeles") 

# Finally, now that we have the local sunrise and sunset times for the date we 
# convert them back to UTC times
sunriseUTC <- dateInfo$sunrise
sunsetUTC <- dateInfo$sunset
attributes(sunriseUTC)$tzone <- "UTC"
attributes(sunsetUTC)$tzone <- "UTC"

# Round and contract the boundary hours
sunriseHourUTC <- lubridate::ceiling_date(sunriseUTC, unit = "hour")
#sunsetHourUTC <- lubridate::floor_date(sunsetUTC, unit = "hour")
sunsetHourUTC <- sunriseHourUTC + lubridate::hours(4) # Shorter period for debugging

# Get all the UTC hours between the local sunrise and sunset hours
hours <- seq.POSIXt(from = sunriseHourUTC, to = sunsetHourUTC, by = "hour")
hours <- strftime(hours, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Create a rasterStack for each hour and add them all to one big "day" rasterStack
dayStack <- raster::stack()
for (hour in hours) {
  startTime <- Sys.time()
  
  result <- try({
    hourStack <- goesaodc_createHourlyRasterStack(startdate = hour,
                                                  res = 0.1,
                                                  bbox = bbox_oregon)
    
    # Combine the rasters and timestamps of the day and hour stacks
    zDay <- raster::getZ(dayStack)
    zHour <- raster::getZ(hourStack)
    
    dayStack <- raster::stack(dayStack, hourStack)
    dayStack <- raster::setZ(dayStack, c(zDay, zHour))
  }, silent = TRUE)
  
  enddtime <- Sys.time()
  print(paste(hour, "took", difftime(enddtime, startTime, units = "secs"), "seconds"))
}

tb <- raster_createLocationTimeseries(dayStack, longitude = lon, latitude = lat, bbox = bbox_oregon)
attributes(tb$datetime)$tzone <- "America/Los_Angeles"
rasterAvg <- raster::mean(dayStack, na.rm = TRUE)

# Plot AOD time series
pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
par(mfrow = c(1, 2))
raster::plot(rasterAvg, col = pal_aod(50), 
             main = "Average AOD", xlab = "Longitude", ylab = "Latitude",
             xlim = c(-125, -122), ylim = c(42, 44))
plot(oregon, add = TRUE)
points(x = c(lon), y = c(lat), cex = 2.0, pch = 3, lwd = 2)
plot(x = tb$datetime, y = tb$aod, 
     pch = 15, cex = 0.8, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.8),
     main = dateLocal, xlab = "Time (local)", ylab = "AOD")
