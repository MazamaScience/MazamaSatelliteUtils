library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)
setSatelliteDataDir("~/Data/Satellite")
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")

# Oregon on July 31, 2019 at 9am (the Milepost 97 Fire)
bbox_oregon <- sp::bbox(subset(USCensusStates, stateCode == "OR"))
date <- lubridate::ymd_h("2019-07-31 16", tz = "UTC")

rstrStack <- goesaodc_createHourlyRasterStack(startdate = date, 
                                              bbox = bbox_oregon,
                                              res = 0.1)

# Set sample location coordinates
location <- data.frame(lon = -123.296895, lat = 42.858787)
datetime <- c()
aod <- c()

for ( i in c(1:length(rstrStack[1])) ) {
  layer <- rstrStack[[i]]
  datetime[i] <- names(layer)
  # method="bilinear" interpolates from the 4 closest cells
  aod[i] <- raster::extract(layer, location, method = "simple")
}

datetime <- paste0(lubridate::year(date), "-", lubridate::month(date), "-", 
                   lubridate::day(date), datetime)
datetime <- as.POSIXct(datetime, format = "%Y-%m-%dX%H.%M.%S", 
                       tz = "America/Los_Angeles")

df <- data.frame(datetime, aod)

plot(x = df$datetime, y = df$aod, type = "l", 
     main = paste0("Milepost 97 (", date, ")"), xlab = "Time", ylab = "AOD")
