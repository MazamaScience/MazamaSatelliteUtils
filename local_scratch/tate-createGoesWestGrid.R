library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")
nc17 <- goesaodc_openFile("OR_ABI-L2-AODC-M6_G17_s20192440001197_e20192440003570_c20192440006107.nc")

coordGrid <- goesaodc_getCoordGrid(nc17)

projection <- goesaodc_getProjection(nc17)
longitude <- coordGrid$lon
latitude <- coordGrid$lat

goesWestGrid <- list(longitude = matrix(longitude, nrow = 2500, ncol = 1500),
                     latitude = matrix(latitude, nrow = 2500, ncol = 1500), 
                     projection = projection)