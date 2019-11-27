# Cave fire region near Santa Barbara, CA

library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

netCDF <- goesaodc_downloadAOD(
 satID = "G17", 
 datetime = "2019-11-26 8", 
 timezone = "America/Los_Angeles",
 verbose = TRUE)[6]
   
kincade_bbox <- c(-125, -116, 33, 37)   
 
nc <- goesaodc_openFile(netCDF)
sp <- goesaodc_createSpatialPoints(nc, dqfLevel = 3, bbox = kincade_bbox)
maps::map(database = "state", "regions" = c("california"), 
          xlim = c(-125, -116.5), ylim = c(33, 37))
goesaodc_plotSpatialPoints(sp, cex = 0.35, add = TRUE)
