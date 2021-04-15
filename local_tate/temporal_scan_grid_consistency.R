# Let's Check if scan coordinates are consistent across multiple scans.
# We'll gather the scan coordinates for scans across one hour and compare:
#   1. The dimensions of the reading tibbles
#   2. The min/max lon/lat coords of the scan
#
# If all of these are consistent, then temporal aggregation between scans should
# be relatively straightforward.

library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

# Kincade fire region
kincade_bbox <- c(-124, -120, 36, 39)

# G17 scans during 10am on Oct 27, 2019
files <- goesaodc_downloadAOD(
  satID = "G17", 
  datetime = "2019-10-27 10", 
  timezone = "America/Los_Angeles"
)

# Create a netCDF handle for each scan file
ncs <- list()
for ( file in files ) {
  
  label <- 
    file %>%
    goesaodc_convertFilenameToDatetime() %>%
    MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
  
  ncs[[label]] <- goesaodc_openFile(basename(file))
  
}

# Define a version of goesaodc_createTibble() that DOESN"T drop NAs
createTibble <- function(nc) {
  nativeGrid <- goesaodc_createNativeGrid(nc, bbox_CONUS)
  
  varList <- list()
  varList[["lon"]] <- as.numeric(nativeGrid$lon)
  varList[["lat"]] <- as.numeric(nativeGrid$lat)
  varList[["AOD"]] <- as.numeric(nativeGrid$AOD)
  varList[["DQF"]] <- as.numeric(nativeGrid$DQF)
  
  tbl <- tibble::as_tibble(varList)
  
  return(tbl)
}

# Create a tibble for each scan
tbs <- list()
for ( nc in ncs ) {
  
  label <- 
    basename(nc$filename) %>%
    goesaodc_convertFilenameToDatetime() %>%
    MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
  
  tbs[[label]] <- createTibble(nc)
  
}

# Compare each scan's grid dimensions
firstGridDim <- dim(tbs[[1]])
for ( tb in tbs ) {
  gridDim <- dim(tb)
  print(gridDim)
}

# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4
# [1] 870060      4

# Compare each scan's grid boundary coords
firstGridBounds <- c(
  min(tbs[[1]]$lon),
  max(tbs[[1]]$lon),
  min(tbs[[1]]$lat),
  max(tbs[[1]]$lat)
)
for ( tb in tbs ) {
  gridBounds <- c(
    min(tb$lon),
    max(tb$lon),
    min(tb$lat),
    max(tb$lat)
  )
  print(gridBounds)
}

# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236
# [1] -129.01980  -91.53395   23.61288   52.31236

# Conclusion:
# It looks like scan coordinates are consistent enough between scans that we can
# do temporal aggregation (since we can count on AOD1[1,1] describing the same 
# location as AOD2[1,1]).