
## This is the code that builds the dataset. It is saved here but not run. Instead, files are loaded locally. 

createNapaFiresData <- function(outputDir, maiacDataDir){
  
  # Load all the MAIAC data
  oct08A <- maiac_loadRaster("h01v04", 20171008, 2105,  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>%
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct08T <- maiac_loadRaster("h01v04", 20171008, 1930, product = "MAIACTAOT",  maiacDataDir = maiacDataDir) %>% 
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct09A <- maiac_loadRaster("h01v04", 20171009, 2150,  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct09T <- maiac_loadRaster("h01v04", 20171009, product = "MAIACTAOT",  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))%>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct10A <- maiac_loadRaster("h01v04", 20171010,  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct10T <- maiac_loadRaster("h01v04", 20171010, 1915, product = "MAIACTAOT",  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct11A <- maiac_loadRaster("h01v04", 20171011, 2135,  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct11T <- maiac_loadRaster("h01v04", 20171011, 1820, product = "MAIACTAOT",  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct12A <- maiac_loadRaster("h01v04", 20171012, 2040,  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct12T <- maiac_loadRaster("h01v04", 20171012, product = "MAIACTAOT",  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct13A <- maiac_loadRaster("h01v04", 20171013, maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct13T <- maiac_loadRaster("h01v04", 20171013, 1945, product = "MAIACTAOT",  maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  
  oct08A_height <- maiac_loadRaster("h01v04", 20171008, 2105, param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>%
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct08T_height <- maiac_loadRaster("h01v04", 20171008, 1930, product = "MAIACTAOT", param = "Injection_Height", maiacDataDir = maiacDataDir) %>% 
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct09A_height <- maiac_loadRaster("h01v04", 20171009, 2150,  param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct09T_height <- maiac_loadRaster("h01v04", 20171009, product = "MAIACTAOT", param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>%
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct10A_height <- maiac_loadRaster("h01v04", 20171010,  param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct10T_height <- maiac_loadRaster("h01v04", 20171010, 1915, product = "MAIACTAOT", param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct11A_height <- maiac_loadRaster("h01v04", 20171011, 2135, param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct11T_height <- maiac_loadRaster("h01v04", 20171011, 1820, product = "MAIACTAOT", param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct12A_height <- maiac_loadRaster("h01v04", 20171012, 2040, param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78)) 
  oct12T_height <- maiac_loadRaster("h01v04", 20171012, product = "MAIACTAOT", param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct13A_height <- maiac_loadRaster("h01v04", 20171013, param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  oct13T_height <- maiac_loadRaster("h01v04", 20171013, 1945, product = "MAIACTAOT", param = "Injection_Height", maiacDataDir = maiacDataDir) %>%
    projectRaster(crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>% 
    crop(raster::extent(-125.52, -117.03, 36.52, 41.78))
  
  
  # load monitoring data
  airnow <- airnow_load(2017)
  wrcc <- wrcc_load(2017)
  airsis <- airsis_load(2017)
  all_monitors <- monitor_combine(list(airnow, wrcc, airsis))
  monitors <- monitor_subset(all_monitors, xlim = c(-125.52, -117.03), ylim = c(36.52, 41.78))
  fireMonitors <- monitor_subset(monitors, tlim = c(20171008, 20171014))
  
  
  # Define folder to save .Rdata files
  filePath <- outputDir
  
  # define blank datafram to insert data into
  dfa <- data_frame(monitorID = vector("character"),
                    monitor = vector("numeric"),
                    satellite = vector("numeric"),
                    day = vector("numeric"),
                    instrument = vector("character"),
                    rasterIndex = vector("numeric"),
                    longitude = vector("numeric"),
                    latitude = vector("numeric"),
                    height = vector("numeric"),
                    sd3 = vector("numeric"),
                    sd5 = vector("numeric"),
                    aod_3 = vector("logical"),
                    aod_5 = vector("logical"),
                    height_3 = vector("logical"),
                    height_5 = vector("logical"))
  dft <- dfa
  monitorCount <- nrow(fireMonitors$meta)
  
  rasterList <- list(oct08A, oct08T, oct08A_height, oct08T_height, oct09A, oct09T, oct09A_height, oct09T_height, 
                     oct10A, oct10T, oct10A_height, oct10T_height, oct11A, oct11T, oct11A_height, oct11T_height, 
                     oct12A, oct12T, oct12A_height, oct12T_height, oct13A, oct13T, oct13A_height, oct13T_height)
  
  # Get the data for each day
  for (i in 1:6) {
    
    # Select the correct rasters
    AQUA <- rasterList[[i*4 - 3]]
    names(AQUA) <- "AQUA"
    TERRA <- rasterList[[i*4-2]]
    names(TERRA) <- "TERRA"
    AQUA_height <- rasterList[[i*4-1]]
    names(AQUA_height) <- "AQUA_height"
    TERRA_height <- rasterList[[i*4]]
    names(TERRA_height) <- "TERRA_height"
    
    # Add additional layers
    for (satellite in list(AUQA = AQUA, TERRA = TERRA, AQUA_height = AQUA_height, TERRA_height = TERRA_height) ) {
      
      # Remove water values
      land <- mask(satellite, subset(USCensusStates, stateCode %in% c("CA", "NV")))
      
      # Fill in some missing values and add mask for where they are
      mean3 <- focal(land, w = matrix(1,3,3), fun = mean, na.rm = TRUE)
      mean5 <- focal(land, w = matrix(1,5,5), fun = mean, na.rm = TRUE)
      mean3mask <- setValues(land, ifelse(is.na(values(land)) & !is.na(values(mean3)), TRUE, FALSE))
      mean5mask <- setValues(land, ifelse(is.na(values(land)) & is.na(values(mean3)) & !is.na(values(mean5)), TRUE, FALSE))
      sat3 <- cover(land, mean3)
      sat5 <- cover(sat3, mean5)
      
      stack <- addLayer(satellite, land, mean3mask, mean5mask, sat5)
      names(stack) <- c(names(satellite), "land", "mean3mask", "mean5mask", "corrected")
      assign(names(satellite), stack)
    }
    
    for (satellite in list(AQUA = AQUA, TERRA = TERRA)) {
      # Calculate 'rumple' index 
      rumpled3 <- focal(satellite$land, w=matrix(1,3,3), fun=sd, na.rm = TRUE)
      rumpled5 <- focal(satellite$land, w=matrix(1, 5, 5), fun=sd, na.rm = TRUE)
      
      stack <- addLayer(satellite, rumpled3, rumpled5)
      names(stack) <- c(names(satellite), "sd3", "sd5")
      assign(names(stack)[1], stack)
    }
    
    
    # Get the day of month as a zero-padded string
    day <- stringr::str_pad(i+7, 2, "left", "0")
    
    for (satellite in list(AUQA = AQUA, TERRA = TERRA, AQUA_height = AQUA_height, TERRA_height = TERRA_height) ){
      fileName <- paste0(names(satellite)[1], "_", day)
      assign(fileName, satellite)
      save(list = fileName, file = file.path(filePath, paste0(fileName, ".Rdata") ) )
    }
    
    # Make blank dataframes for this day
    aquadf <- data_frame(monitorID = fireMonitors$meta$monitorID, 
                         monitor = vector("numeric", monitorCount),
                         satellite = vector("numeric", monitorCount), 
                         day = vector("numeric", monitorCount),
                         instrument = vector("character", monitorCount),
                         rasterIndex = vector("numeric", monitorCount),
                         longitude = vector("numeric", monitorCount),
                         latitude = vector("numeric", monitorCount), 
                         height = vector("numeric", monitorCount),
                         aod_3 = vector("logical", monitorCount),
                         aod_5 = vector("logical", monitorCount),
                         height_3 = vector("logical", monitorCount),
                         height_5 = vector("logical", monitorCount),
                         sd3 = vector("numeric", monitorCount),
                         sd5 = vector("numeric", monitorCount))
    terradf <- aquadf
    
    # Get the the index for the correct day and time
    timeIndexA <- which(fireMonitors$data$datetime == lubridate::ymd_h(paste0("201710", day, "21")) )
    timeIndexT <- which(fireMonitors$data$datetime == lubridate::ymd_h(paste0("201710", day, "19")) )
    
    # Add monitor and satellite data for each monitor
    for (i in 1:monitorCount) {
      monitorInfo <- fireMonitors$meta[i,]
      aquadf[i,'monitor'] <- fireMonitors$data[timeIndexA, monitorInfo$monitorID]
      aquadf[i, 'satellite'] <- getValue(AQUA$corrected, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[i, 'datetime'] <- lubridate::ymd_h(paste0("201710", day, "21"))
      aquadf[i, 'rasterIndex'] <- raster::cellFromXY(AQUA, c(monitorInfo$longitude, monitorInfo$latitude))
      aquadf[i, 'latitude'] <- monitorInfo$latitude
      aquadf[i, 'longitude'] <- monitorInfo$longitude
      aquadf[i, 'height'] <- getValue(AQUA_height$corrected, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[i, 'aod_3'] <- getValue(AQUA$mean3mask, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[i, 'aod_5'] <- getValue(AQUA$mean5mask, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[i, 'height_3'] <- getValue(AQUA_height$mean3mask, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[i, 'height_5'] <- getValue(AQUA_height$mean5mask, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[i, 'sd3'] <- getValue(AQUA$sd3, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[i, 'sd5'] <- getValue(AQUA$sd5, monitorInfo$longitude, monitorInfo$latitude)
      terradf[i,'monitor'] <- fireMonitors$data[timeIndexT, monitorInfo$monitorID]
      terradf[i, 'satellite'] <- getValue(TERRA$corrected, monitorInfo$longitude, monitorInfo$latitude)
      terradf[i, 'datetime'] <- lubridate::ymd_h(paste0("201710", day, "19")) 
      terradf[i, 'rasterIndex'] <- raster::cellFromXY(TERRA, c(monitorInfo$longitude, monitorInfo$latitude))
      terradf[i, 'latitude'] <- monitorInfo$latitude
      terradf[i, 'longitude'] <- monitorInfo$longitude
      terradf[i, 'height'] <- getValue(TERRA_height$corrected, monitorInfo$longitude, monitorInfo$latitude)
      terradf[i, 'aod_3'] <- getValue(TERRA$mean3mask, monitorInfo$longitude, monitorInfo$latitude)
      terradf[i, 'aod_5'] <- getValue(TERRA$mean5mask, monitorInfo$longitude, monitorInfo$latitude)
      terradf[i, 'height_3'] <- getValue(TERRA_height$mean3mask, monitorInfo$longitude, monitorInfo$latitude)
      terradf[i, 'height_5'] <- getValue(TERRA_height$mean5mask, monitorInfo$longitude, monitorInfo$latitude)
      terradf[i, 'sd3'] <- getValue(TERRA$sd3, monitorInfo$longitude, monitorInfo$latitude)
      terradf[i, 'sd5'] <- getValue(TERRA$sd5, monitorInfo$longitude, monitorInfo$latitude)
    }
    
    # Add that day's worth of data to full data frame
    dfa <- add_row(dfa, monitorID = aquadf$monitorID, satellite = aquadf$satellite, monitor = aquadf$monitor, 
                   day = day, instrument = "aqua", rasterIndex = aquadf$rasterIndex, longitude = aquadf$longitude, 
                   latitude = aquadf$latitude, height = aquadf$height, aod_3 = aquadf$aod_3, aod_5 = aquadf$aod_5, 
                   height_3 = aquadf$height_3, height_5 = aquadf$height_5, sd3 = aquadf$sd3, sd5 = aquadf$sd5)
    dft <- add_row(dft, monitorID = terradf$monitorID, satellite = terradf$satellite, monitor = terradf$monitor, 
                   day = day, instrument = "terra", rasterIndex = terradf$rasterIndex, longitude = terradf$longitude, 
                   latitude = terradf$latitude, height = terradf$height, aod_3 = terradf$aod_3, aod_5 = terradf$aod_5, 
                   height_3 = terradf$height_3, height_5 = terradf$height_5, sd3 = terradf$sd3, sd5 = terradf$sd5)
  }
  
  # Get data frame with data from both satellites
  dfall <- add_row(dft, monitorID = dfa$monitorID, satellite = dfa$satellite, monitor = dfa$monitor, day = dfa$day, instrument = dfa$instrument, rasterIndex = dfa$rasterIndex, longitude = dfa$longitude, latitude = dfa$latitude, height = dfa$height, aod_3 = dfa$aod_3, aod_5 = dfa$aod_5, height_3 = dfa$height_3, height_5 = dfa$height_5)
  
  # For height, make all 'na' values 0. 
  dfall$height <- ifelse(is.na(dfall$height), 0, dfall$height)
  
  save(dfall, file = file.path(filePath, "maiac_dataframe.Rdata"))
}

