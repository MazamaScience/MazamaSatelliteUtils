
## This is the code that builds the dataset. It is saved here but not run. Instead, files are loaded locally. 

createChetcoData <- function(outputDir, maiacDataDir){
  
  # Load all the MAIAC data
  
  # sept 01
  sep01An <- maiac_loadRaster("h01v03", 20170901, 2050) 
  sep01As <- maiac_loadRaster("h01v04", 20170901, 2045)
  sep01A <- raster::merge(sep01An, sep01As)
  
  sep01Tn <- maiac_loadRaster("h01v03", 20170901, 1905, product = "MAIACTAOT") 
  sep01Ts <- maiac_loadRaster("h01v04", 20170901, product = "MAIACTAOT")
  sep01T <- raster::merge(sep01Tn, sep01Ts)
  
  # sept 02
  sep02An <- maiac_loadRaster("h01v03", 20170902, 2130) 
  sep02As <- maiac_loadRaster("h01v04", 20170902, 2130)
  sep02A <- raster::merge(sep02An, sep02As)
  
  sep02Tn <- maiac_loadRaster("h01v03", 20170902, 1950, product = "MAIACTAOT") 
  sep02Ts <- maiac_loadRaster("h01v04", 20170902, 1950, product = "MAIACTAOT")
  sep02T <- raster::merge(sep02Tn, sep02Ts)
  
  # sept 03
  sep03An <- maiac_loadRaster("h01v03", 20170903, 2035) 
  sep03As <- maiac_loadRaster("h01v04", 20170903, 2035)
  sep03A <- raster::merge(sep03An, sep03As)
  
  sep03Tn <- maiac_loadRaster("h01v03", 20170903, 1855, product = "MAIACTAOT") 
  sep03Ts <- maiac_loadRaster("h01v04", 20170903, product = "MAIACTAOT")
  sep03T <- raster::merge(sep03Tn, sep03Ts)
  
  # sept 04
  sep04An <- maiac_loadRaster("h01v03", 20170904, 2120)
  sep04As <- maiac_loadRaster("h01v04", 20170904)
  sep04A <- raster::merge(sep04An, sep04As)
  
  sep04Tn <- maiac_loadRaster("h01v03", 20170904, 1940, product = "MAIACTAOT") 
  sep04Ts <- maiac_loadRaster("h01v04", 20170904, 1940, product = "MAIACTAOT")
  sep04T <- raster::merge(sep04Tn, sep04Ts)
  
  # sept 05
  sep05An <- maiac_loadRaster("h01v03", 20170905, 2205)
  sep05As <- maiac_loadRaster("h01v04", 20170905, 2200)
  sep05A <- raster::merge(sep05An, sep05As)
  
  sep05Tn <- maiac_loadRaster("h01v03", 20170905, 1845, product = "MAIACTAOT") 
  sep05Ts <- maiac_loadRaster("h01v04", 20170905, 1845, product = "MAIACTAOT")
  sep05T <- raster::merge(sep05Tn, sep05Ts)
  
  # sept 06
  sep06An <- maiac_loadRaster("h01v03", 20170906, 2110)
  sep06As <- maiac_loadRaster("h01v04", 20170906, 2105)
  sep06A <- raster::merge(sep06An, sep06As)
  
  sep06Tn <- maiac_loadRaster("h01v03", 20170906, 1925, product = "MAIACTAOT") 
  sep06Ts <- maiac_loadRaster("h01v04", 20170906, 1930, product = "MAIACTAOT")
  sep06T <- raster::merge(sep06Tn, sep06Ts)
  
  # sept 07
  sep07An <- maiac_loadRaster("h01v03", 20170907, 2150)
  sep07As <- maiac_loadRaster("h01v04", 20170907, 2150)
  sep07A <- raster::merge(sep07An, sep07As)
  
  sep07Tn <- maiac_loadRaster("h01v03", 20170907, 1830, product = "MAIACTAOT") 
  sep07Ts <- maiac_loadRaster("h01v04", 20170907, product = "MAIACTAOT")
  sep07T <- raster::merge(sep07Tn, sep07Ts)
  
  # sept 08
  sep08An <- maiac_loadRaster("h01v03", 20170908, 2055)
  sep08As <- maiac_loadRaster("h01v04", 20170908, 2055)
  sep08A <- raster::merge(sep08An, sep08As)
  
  sep08Tn <- maiac_loadRaster("h01v03", 20170908, 1915, product = "MAIACTAOT") 
  sep08Ts <- maiac_loadRaster("h01v04", 20170908, 1915, product = "MAIACTAOT")
  sep08T <- raster::merge(sep08Tn, sep08Ts)
  
  # Stick them all together
  maiac_stack <- stack(list(sep01A = sep01A, sep01T = sep01T, sep02A = sep02A, sep02T = sep02T, 
                            sep03A = sep03A, sep03T = sep03T, sep04A = sep04A, sep04T = sep04T, 
                            sep05A = sep05A, sep05T = sep05T, sep06A = sep06A, sep06T = sep06T, 
                            sep07A = sep07A, sep07T = sep07T, sep08A = sep08A, sep08T = sep08T))
  
  # Reproject and crop
  maiac_fixed <- projectRaster(maiac_stack, crs = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")) %>%
    crop(extent(-125, -118, 39, 45))
  
  
  # load monitoring data
  airnow <- airnow_load(2017)
  wrcc <- wrcc_load(2017)
  airsis <- airsis_load(2017)
  all_monitors <- monitor_combine(list(airnow, wrcc, airsis))
  monitors <- monitor_subset(all_monitors, xlim = c(-125, -118), ylim = c(39, 45))
  fireMonitors <- monitor_subset(monitors, tlim = c(20170901, 20170909))
  
  
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
  
  
  # Get the data for each day
  for (i in 1:8) {
    
    # Get day as 0-padded string
    day <- stringr::str_pad(i, 2, "left", "0")
    
    # Select the correct rasters
    AQUA <- maiac_fixed[[i*2 - 1]]
    names(AQUA) <- "AQUA"
    TERRA <- maiac_fixed[[i*2]]
    names(TERRA) <- "TERRA"
    
    # Add additional layers
    for (satellite in list(AUQA = AQUA, TERRA = TERRA) ) {
      
      # Remove water values
      land <- mask(satellite, subset(USCensusStates, stateCode %in% c("CA", "NV", "OR")))
      
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
      
      # Calculate 'rumple' index 
      rumpled3 <- focal(stack$land, w=matrix(1,3,3), fun=sd, na.rm = TRUE)
      rumpled5 <- focal(stack$land, w=matrix(1, 5, 5), fun=sd, na.rm = TRUE)
      
      stack <- addLayer(stack, rumpled3, rumpled5)
      names(stack)[6:7] <- c( "sd3", "sd5")
      
      fileName <- paste0("chetco_", names(satellite)[1], "_", day)
      assign(fileName, stack)
      assign(names(stack)[1], stack)
      save(list = fileName, file = file.path(filePath, paste0(fileName, ".Rdata") ) )
      
      print(paste0("finished ", names(stack)[1]))
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
    timeIndexA <- which(fireMonitors$data$datetime == lubridate::ymd_h(paste0("201709", day, "21")) )
    timeIndexT <- which(fireMonitors$data$datetime == lubridate::ymd_h(paste0("201709", day, "19")) )
    
    # Add monitor and satellite data for each monitor
    for (j in 1:monitorCount) {
      monitorInfo <- fireMonitors$meta[j,]
      aquadf[j,'monitor'] <- fireMonitors$data[timeIndexA, monitorInfo$monitorID]
      aquadf[j, 'satellite'] <- getValue(AQUA$corrected, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[j, 'datetime'] <- lubridate::ymd_h(paste0("201710", day, "21"))
      aquadf[j, 'rasterIndex'] <- raster::cellFromXY(AQUA, c(monitorInfo$longitude, monitorInfo$latitude))
      aquadf[j, 'latitude'] <- monitorInfo$latitude
      aquadf[j, 'longitude'] <- monitorInfo$longitude
      aquadf[j, 'aod_3'] <- getValue(AQUA$mean3mask, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[j, 'aod_5'] <- getValue(AQUA$mean5mask, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[j, 'sd3'] <- getValue(AQUA$sd3, monitorInfo$longitude, monitorInfo$latitude)
      aquadf[j, 'sd5'] <- getValue(AQUA$sd5, monitorInfo$longitude, monitorInfo$latitude)
      terradf[j,'monitor'] <- fireMonitors$data[timeIndexT, monitorInfo$monitorID]
      terradf[j, 'satellite'] <- getValue(TERRA$corrected, monitorInfo$longitude, monitorInfo$latitude)
      terradf[j, 'datetime'] <- lubridate::ymd_h(paste0("201710", day, "19")) 
      terradf[j, 'rasterIndex'] <- raster::cellFromXY(TERRA, c(monitorInfo$longitude, monitorInfo$latitude))
      terradf[j, 'latitude'] <- monitorInfo$latitude
      terradf[j, 'longitude'] <- monitorInfo$longitude
      terradf[j, 'aod_3'] <- getValue(TERRA$mean3mask, monitorInfo$longitude, monitorInfo$latitude)
      terradf[j, 'aod_5'] <- getValue(TERRA$mean5mask, monitorInfo$longitude, monitorInfo$latitude)
      terradf[j, 'sd3'] <- getValue(TERRA$sd3, monitorInfo$longitude, monitorInfo$latitude)
      terradf[j, 'sd5'] <- getValue(TERRA$sd5, monitorInfo$longitude, monitorInfo$latitude)
      
      print(paste0("finished monitor ", j))
    }
    
    # Add that day's worth of data to full data frame
    dfa <- add_row(dfa, monitorID = aquadf$monitorID, satellite = aquadf$satellite, monitor = aquadf$monitor, 
                   day = day, instrument = "aqua", rasterIndex = aquadf$rasterIndex, longitude = aquadf$longitude, 
                   latitude = aquadf$latitude, aod_3 = aquadf$aod_3, aod_5 = aquadf$aod_5, 
                   sd3 = aquadf$sd3, sd5 = aquadf$sd5)
    dft <- add_row(dft, monitorID = terradf$monitorID, satellite = terradf$satellite, monitor = terradf$monitor, 
                   day = day, instrument = "terra", rasterIndex = terradf$rasterIndex, longitude = terradf$longitude, 
                   latitude = terradf$latitude,  aod_3 = terradf$aod_3, aod_5 = terradf$aod_5, 
                     sd3 = terradf$sd3, sd5 = terradf$sd5)
    
    
    print(paste0("finished Sep ", day))
  }
  
  # Get data frame with data from both satellites
  chetcodf <- add_row(dft, monitorID = dfa$monitorID, satellite = dfa$satellite, monitor = dfa$monitor, 
                   day = dfa$day, instrument = dfa$instrument, rasterIndex = dfa$rasterIndex, 
                   longitude = dfa$longitude, latitude = dfa$latitude, 
                   aod_3 = dfa$aod_3, aod_5 = dfa$aod_5)
  
  save(chetcodf, file = file.path(filePath, "chetco_maiac_dataframe.Rdata"))
}

