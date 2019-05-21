# Tom's Scratch
# 5/20/19

# ----- Create a RasterStack from Camp Fire AOD data ---------------------------

# On Friday my analysis of AOD data from the Camp Fire last year got hung up on 
# the first step: creating a RasterStack from the data I downloaded from NOAA

# Heres where I was:

setSatelliteDataDir("./local_data/CampFire/001")

# Get a list of scan start times for each file in this dataset
times <-
  list.files(getSatelliteDataDir()) %>%
  purrr::map(goesaodc_getStartString) %>%
  purrr::map(lubridate::parse_date_time, orders = "YjHMS", tz = "UTC")

# first file in california local time
firstTime <- times[[1]]
firstTime
# [1] "2018-11-15 14:02:15 UTC"
lubridate::with_tz(firstTime, "US/Pacific")
# [1] "2018-11-15 06:02:15 PST"

lastTime <- times[[length(times)]]
lastTime
# [1] "2018-11-16 00:57:15 UTC"
lubridate::with_tz(lastTime, "US/Pacific")
# [1] "2018-11-15 16:57:15 PST"

# We can see that we have data from 6 AM to 5 PM on November 15th 2018
# These are approximately sunrise and sunset

# Ok lets just try making a RasterStack for the first hour

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")

# get bounding box for California
loadSpatialData("USCensusStates")
ca <- subset(USCensusStates, stateCode == "CA")
bb_ca <- bbox(ca)

startdate <- "2018111514"
rstStack <- goesaodc_createHourlyRasterStack(startdate, bbox = bb_ca)
# Generates error: No data for selected region

# Looks like there isn't any AOD data in one of the 6 AM files. 

# TODO: Handle that error inside goesaodc_createHourlyRasterStack so that one empty
# TODO: file doesn't break the function for that whole hour.

# TODO: Per Jon's request, a function that creates a facet plot or "small multiples"
# TODO: for an hour using just SpatialPoints objects (without rasterizing)

# For now, lets just keep trying to find an hour that we can build a RasterStack
# for.

startdate <- "2018111515"
rstStack <- goesaodc_createHourlyRasterStack(startdate, bbox = bb_ca)
# Generates error: Error in compareRaster(x) : different number or columns

# Ok so looks like the files for 7AM all have at least some data for
# California but we are running into a different problem.

# Lets try comparing the rasters from 7AM

# Get a list of Rasters for 7AM
rasters <-
  goesaodc_listFiles(startdate) %>%
  purrr::map(goesaodc_openFile) %>%
  purrr::map(goesaodc_createRaster, bbox = bb_ca)

# List rows/columns for each raster
purrr::map(rasters, function(rst) dim(rst$AOD))

# [[1]]
# [1]  92 102   1
# [[2]]
# [1]  92 102   1
# [[3]]
# [1]  92 103   1
# [[4]]
# [1]  95 103   1
# [[5]]
# [1]  95 103   1
# [[6]]
# [1]  94 103   1
# [[7]]
# [1]  94 103   1
# [[8]]
# [1]  94 103   1
# [[9]]
# [1]  95 103   1
# [[10]]
# [1]  95 103   1
# [[11]]
# [1]  95 103   1
# [[12]]
# [1]  95 103   1

# Ok, so there are some small variations in the dimentions of the rasters for
# this hour. Why would that be?

# Maybe the extents of the rasters are different enough that rasterize actually
# generates different sized grids. If that is the case, setting the extent of
# the SpatialPoints objects explicitly inside goesaodc_createSpatialPoints
# should remidy the problem.
