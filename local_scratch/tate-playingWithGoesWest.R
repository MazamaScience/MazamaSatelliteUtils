fileEast <- goesaodc_listFiles("G16", 2019090118)[1]
fileWest <- goesaodc_listFiles("G17", 2019090118)[1]

ncEast <- goesaodc_openFile(fileEast)
ncWest <- goesaodc_openFile(fileWest)

breaks <- c(-3.0, -1.0, 0.0, 0.1, 0.3, 0.5, 0.8, 1.0, 1.5, 3.0)

# All quality coverage
spEast <- goesaodc_createSpatialPoints(ncEast, dqfLevel = 2)
spWest <- goesaodc_createSpatialPoints(ncWest, dqfLevel = 2)

plot(0, 0, xlim = c(-180, -50), ylim = c(10, 60))
goesaodc_plotSpatialPoints(spEast, breaks = breaks, cex = 0.1, add = TRUE)
goesaodc_plotSpatialPoints(spWest, breaks= breaks, cex = 0.1, add = TRUE)
maps::map(add = TRUE)
maps::map("state", add = TRUE)

# High quality coverage
spEast <- goesaodc_createSpatialPoints(ncEast, dqfLevel = 1)
spWest <- goesaodc_createSpatialPoints(ncWest, dqfLevel = 1)

plot(0, 0, xlim = c(-180, -50), ylim = c(10, 60))
goesaodc_plotSpatialPoints(spEast, breaks = breaks, cex = 0.1, add = TRUE)
goesaodc_plotSpatialPoints(spWest, breaks= breaks, cex = 0.1, add = TRUE)
maps::map(add = TRUE)
maps::map("state", add = TRUE)

