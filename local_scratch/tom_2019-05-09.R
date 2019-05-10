# Tom's Scratch
# 5/9/2019

# ----- create a raster stack for a specified hour -----------------------------

# yesterday at noon
date <- 20190508
hour <- 12

# make sure files are downloaded
setSatelliteDataDir("~/Data/Satellite")

# check that its daylight
if ( isDaylight(paste0(date,hour)) ){
  goesaodc_downloadAOD(date = date, hour = hour)
} else {
  print("Selected hour is not during daylight hours in CONUS")
}

# create bbox for region of interest
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")
pa <- subset(USCensusStates, stateCode == "PA")
bb_pa <- bbox(pa)

# create list of AOD raster layers for specified hour and region
raster_list <- 
  goesaodc_listFiles(paste0(date, hour)) %>%
  purrr::map(goesaodc_openFile) %>%
  purrr::map(goesaodc_createRaster, bbox = bb_pa) %>%
  purrr::map(function(rst) rst[["AOD"]])

aod_stack <- raster::stack(raster_list)
# produces: Error in compareRaster(x) : different extent

# how different are the extents of the rasters in our list?
extents <- purrr::map(raster_list, function(rst) extent(rst))
range(purrr::map_dbl(extents, function(ext) ext@xmin))
# [1] -80.51916 -80.51757
range(purrr::map_dbl(extents, function(ext) ext@xmax))
# [1] -74.71916 -74.71757
range(purrr::map_dbl(extents, function(ext) ext@ymin))
# [1] 39.74788 39.76934
range(purrr::map_dbl(extents, function(ext) ext@ymax))
# [1] 42.24788 42.26934

# There appears to be small variations in extent from file to file, in this case 
# varying by thousandths of a degree in x and hundredths of a degree in y. 
# This is likely because of missing data along the edge of the bounding box.
# I will use the bounding box defined above to reset the extent of each raster
# in the stack, and then stack() should work.

ext <- extent(bb_pa)
raster_list <- purrr::map(raster_list, function(rst) setExtent(rst, ext))
aod_stack <- raster::stack(raster_list)
class(aod_stack)

# Cool, that worked and now we have a RasterStack of all the files from noon
# yesterday. Lets try out rasterVis!

# set the times of each file
times <-
  goesaodc_listFiles(paste0(date, hour)) %>%
  purrr::map_chr(goesaodc_getStartString) %>%
  purrr::map(lubridate::parse_date_time, orders = ("YjHMS"))

aod_stack <- raster::setZ(aod_stack, times)
names(aod_stack) <- purrr::map_chr(times, function(t) format(t, "%Hh%Mm"))

rasterVis::levelplot(aod_stack)

# for some reason it's prepending X to the RasterLayer names, but other than that
# this looks pretty good!

# now lets try aggregating the stack in different ways. It would be informative
# to see: mean, standard deviation, and count

# we can use the raster::calc() method to achieve this

mean <- raster::calc(aod_stack, function(x) mean(x, na.rm = TRUE))
sd <- raster::calc(aod_stack, function(x) sd(x, na.rm = TRUE))
count <- raster::calc(aod_stack, function(x) sum(!is.na(x)))

rasterVis::levelplot(mean, main="Mean")
rasterVis::levelplot(count, main="Count")
rasterVis::levelplot(sd, main="Standard Deviation")
