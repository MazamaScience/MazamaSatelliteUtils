# Tom's scratch file 
# 5/3/2019

# ----- playing with quantile function -----------------------------------------

# download some GOES AOD data from noon today
date <- 20190503
hour <- 12

goesaodc_downloadAOD(date = date, hour = hour)

# grab the first file and create a SpatialPointsDataFrame
pts <- 
  goesaodc_listFiles(date = date, hour = hour)[1] %>%
  goesaodc_openFile() %>%
  goesaodc_createSpatialPoints()

# get 0%, 25%, 50%, 75%, and 100% quantiles for AOD
qtl <- quantile(pts$AOD)
qtl

# use above quantiles for color breaks in a plot
goesaodc_plotSpatialPoints(pts, breaks = qtl)
maps::map("state", add=T)


# ----- get the mean of all GOES rasters in an hour ----------------------------

# download some GOES AOD data from noon today
date <- 20190503
hour <- 12

goesaodc_downloadAOD(date = date, hour = hour)
