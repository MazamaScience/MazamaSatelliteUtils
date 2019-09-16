# Target functionality for working with satellite data

# ----- Top level map functionality --------------------------------------------

# Gee
setSpatialDataDir('~/Data/Spatial')
loadSpatialData('USCensusStates')
bbox(subset(USCensusStates, stateCode == 'WA'))

# Washington state
bbox <-
  USCensusStates %>%
  subset(stateCode == "WA") %>%
  bbox

xlim <- bbox[1,]
yliml <- bbox[2,]

# Create an hourly average raster object for Washington
aod_wa <- goes_createRaster("AOD", 
                            "2019042311",
                            xlim = xlim,
                            ylim = ylim,
                            resolution = 0.05, 
                            stat = "mean")

goes_map(aod_wa)


# ----- Rasterizing function guts ----------------------------------------------

nc_files <- goes_downloadAOD(2019042311)

# Something like these lines below but we'll actually be crating either a
# RasterStack or a RasterBrick -- see raster package documentation
rasterList <- list()
for (nc_file in nc_files) {
  # use timestamp as name?
  rasterList[[name]] <- goes_createRaster(nc_file, xlim, ylim, resolution = 0.05)
}

# Average together using raster::overlay()

# return a RasterLayer with the average values


# ----- goes_createRaster() guts -----------------------------------------------

# open file
# check projection
# create tibble of AOD, DQF, longitude, latitude
# filter for is.na, xlim, ylim
# create SpatialPoints object from lons and lats
# create Raster object from SpatialPoints object
# create RasterLayer from AOD and Raster
# create RasterLayer from DQF and Raster
# return RasterStack with AOD and DQF




