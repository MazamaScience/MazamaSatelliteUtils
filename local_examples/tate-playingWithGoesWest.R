fileEast <- goesaodc_listFiles(2019090118, satId = 16)[1]
fileWest <- goesaodc_listFiles(2019090118, satId = 17)[1]

ncEast <- goesaodc_openFile(fileEast)
ncWest <- goesaodc_openFile(fileWest)

spEast <- goesaodc_createSpatialPoints(ncEast)
spWest <- goesaodc_createSpatialPoints(ncWest)

plot(0, 0, xlim = c(-180, -50), ylim = c(10, 60))
goesaodc_plotSpatialPoints(spEast, add = TRUE)
goesaodc_plotSpatialPoints(spWest, add = TRUE)
maps::map("state", add = TRUE)
