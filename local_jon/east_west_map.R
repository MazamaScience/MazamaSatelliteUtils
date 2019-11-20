goesaodc_downloadAOD("G16", 2019090118)
goesaodc_downloadAOD("G17", 2019090118)

fileEast <- goesaodc_listFiles("G16", 2019090118)[1]
fileWest <- goesaodc_listFiles("G17", 2019090118)[1]

ncEast <- goesaodc_openFile(fileEast)
ncWest <- goesaodc_openFile(fileWest)

SPDF_East <- goesaodc_createSpatialPoints(ncEast)
SPDF_West <- goesaodc_createSpatialPoints(ncWest)

plot(0, 0, xlim = c(-180, -50), ylim = c(10, 60))
goesaodc_plotSpatialPoints(SPDF_East, add = TRUE)
goesaodc_plotSpatialPoints(SPDF_West, add = TRUE)
maps::map("state", add = TRUE)
