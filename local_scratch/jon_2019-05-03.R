# Jon's use case scenario

library(maps)
library(MazamaSpatialUtils)
library(MazamaSatelliteUtils)

setSpatialDataDir("~/Data/Spatial")
setSatelliteDataDir("~/Data/Satellite")

# get bbox for Texas
loadSpatialData("USCensusStates")
tx <- subset(USCensusStates, stateCode == "TX")
bb_tx <- sp::bbox(tx)

# Make maps for a particular hour
files <- goesaodc_listFiles("2019-04-22 09")  # 09:00 UTC is 04:00 EST

# 12 files so:
layout(matrix(seq(12), 4, 3))

for ( file in files ) {
  
  nc <- goesaodc_openFile(file)
  
  map('world', c('canada', 'usa', 'mexico'), xlim=c(-130,-40), ylim=c(0,60))
  map('state', col = "gray80", add = TRUE)
  nc %>%
    goesaodc_createSpatialPoints() %>% # could pass in bbox here
    goesaodc_plotSpatialPoints(n = 1e4, add = TRUE)
  
  ncdf4::nc_close(nc)
  
}



# So now lets try this for mid-day: 17:00 EST
#new_files <- goesaodc_downloadAOD("20190422", hour = "22")

# Make maps for a particular hour
files <- goesaodc_listFiles("2019-04-22 22")  # 22:00 UTC is 17:00 EST

# 12 files so:
layout(matrix(seq(12), 4, 3))

# NOTE:  This takes longer to plot because createSpatialPoints() takes longer
for ( file in files ) {
  
  nc <- goesaodc_openFile(file)
  
  plot(tx)
  nc %>%
    goesaodc_createSpatialPoints(bb_tx) %>% # could pass in bbox here
    goesaodc_plotSpatialPoints(n = 1e4, add = TRUE)
  
  ncdf4::nc_close(nc)
  
}




