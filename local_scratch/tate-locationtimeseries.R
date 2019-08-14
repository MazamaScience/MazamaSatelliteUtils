library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)
setSatelliteDataDir("~/Data/Satellite")
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")
 
# Oregon on July 31, 2019 at 9am (the Milepost 97 Fire)
startdate <- lubridate::ymd_h("2019-07-31 16", tz = "UTC")
bbox <- sp::bbox(subset(USCensusStates, stateCode == "OR"))

# Gather all the raster layers for the given hour
rstrStack <- goesaodc_createHourlyRasterStack(startdate = startdate, 
                                              lonLo = -124,
                                              lonHi = -123,
                                              latLo = 42,
                                              latHi = 43,
                                              res = 0.05)

# Set sample location coordinates
location <- data.frame(lon = -123.26, lat = 42.86)
datetime <- c()
aod <- c()

# Store the AOD value of the location cell in each layer of the stack
for ( i in c(1:length(rstrStack[1])) ) {
  layer <- rstrStack[[i]]
  datetime[i] <- names(layer)
  aod[i] <- raster::extract(x = layer, y = location, 
                            method = "simple",         # Either "simple" or "bilinear". Can't be "bilinear" if buffer is used
                            buffer = 100,              # Area radius in meters. Why not defaulting to degrees like resolution?
                            fun = mean)                # How to aggregate all the values in the buffer
}

# Format the timestamps
datetime <- paste0(lubridate::year(startdate), "-", 
                   lubridate::month(startdate), "-", 
                   lubridate::day(startdate), datetime)
datetime <- as.POSIXct(datetime, format = "%Y-%m-%dX%H.%M.%S", 
                     tz = "America/Los_Angeles")

tb <- tibble::tibble(datetime, aod)

pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
plot(x = tb$datetime, y = tb$aod, col = pal_aod(50), pch = 15, cex = 2,
     main = paste0("Milepost 97 (", startdate, ")"), 
     xlab = "Time", ylab = "AOD")

