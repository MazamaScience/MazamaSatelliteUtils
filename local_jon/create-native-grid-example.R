#
# Do we need to preserve the matrix representation of the data?
#

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")
loadSpatialData("USCensusCounties")

library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

# ----- Open a netcdf file -----------------------------------------------------

# ncFile <- system.file(
#   "extdata",
#   "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc",
#   package = "MazamaSatelliteUtils"
# )

files <- goesaodc_downloadAOD("G17", 2019102714, timezone = "America/Los_Angeles")
files <- goesaodc_listFiles("G17", 2019102714, timezone = "America/Los_Angeles")

# Get ready to store native grids
nativeGridList <- list()

# Loop over timesteps
for ( i in seq_along(files) ) {
  
  print(paste0("Working on ", files[i], " ..."))
  
  nc <- goesaodc_openFile(basename(files[i]))
  
  # ----- Get grid data ----------------------------------------------------------
  
  # Get satID from netCDF, will be either "G16" or "G17"
  satID <- ncdf4::ncatt_get(nc, varid = 0, attname = "platform_ID")$value
  
  # Choose which gridFile to load based on satID
  if ( satID == "G16") {
    gridFile <- "goesEastGrid.rda"
  } else if ( satID == "G17" ) {
    gridFile <- "goesWestGrid.rda"
  }
  
  # Assemble the correct filepath based on satID and Data directory
  filePath <- file.path(getSatelliteDataDir(), gridFile)
  
  # Test for grid existence and if found, load it. Stop with appropriate message
  # if missing
  if ( file.exists(filePath) ) {
    goesGrid <- get(load(filePath))
  } else {
    stop("Grid file not found. Run 'installGoesGrids()' first")
  }  
  
  # ----- Set up our bbox --------------------------------------------------------
  
  # bbox_oregon <- c(-124.56624, -116.46350, 41.99179, 46.29203)
  # 
  # lonLo <- bbox_oregon[1]
  # lonHi <- bbox_oregon[2]
  # latLo <- bbox_oregon[3]
  # latHi <- bbox_oregon[4]
  
  ca <- subset(USCensusStates, stateCode == "CA")
  bbox <- bbox(ca)
  
  lonLo <- bbox[1]
  lonHi <- bbox[2]
  latLo <- bbox[3]
  latHi <- bbox[4]
  
  # Kincade fire
  lonLo <- -124
  lonHi <- -120
  latLo <- 36
  latHi <- 39
  
  # ----- Name things what they are to ease understanding ------------------------
  
  # Matrices of the same dimensions as AOD and DQF
  lonMatrix <- goesGrid$longitude
  latMatrix <- goesGrid$latitude
  
  # Create a matrix of logicals identifying grid cells within bbox_oregon
  gridMask <-
    lonMatrix >= lonLo &
    lonMatrix <= lonHi &
    latMatrix >= latLo &
    latMatrix <= latHi
  
  # > class(gridMask)
  # [1] "matrix"
  # > summary(as.logical(gridMask))
  # Mode   FALSE    TRUE    NA's 
  # logical 3678696   24142   47162 
  
  gridMask[is.na(gridMask)] <- FALSE
  
  # image(gridMask[200:300,200:300])
  
  # ----- Find the i,j bounding box in curvilinear grid space --------------------
  
  # Find the first row in each column inside the bbox
  iLos <- apply(gridMask, 2, function(x) { min(which(x)) })
  iLo <- min(iLos) # lots of Inf but that's OK
  
  # Last row
  iHis <- apply(gridMask, 2, function(x) { max(which(x)) })
  iHi <- max(iHis) # lots of -Inf but that's OK
  
  # First column
  jLos <- apply(gridMask, 1, function(x) { min(which(x)) })
  jLo <- min(jLos) # lots of Inf but that's OK
  
  # Last column
  jHis <- apply(gridMask, 1, function(x) { max(which(x)) })
  jHi <- max(jHis) # lots of -Inf but that's OK
  
  # Convert to the variables we pass to ncvar_get()
  start_x <- iLo
  count_x <- iHi - iLo + 1
  
  start_y <- jLo
  count_y <- jHi - jLo + 1
  
  # ----- Code MODIFIED from goesaodc_createTibble() -----------------------------
  
  nativeGrid <- list()
  
  # Get subset lons and lats from the original grid file
  nativeGrid[["lon"]] <- lonMatrix[iLo:iHi,jLo:jHi]
  nativeGrid[["lat"]] <- latMatrix[iLo:iHi,jLo:jHi]
  
  # Get AOD using start and count arguments
  nativeGrid[["AOD"]] <- ncdf4::ncvar_get(
    nc, 
    varid = "AOD",
    start = c(start_x, start_y),
    count = c(count_x, count_y),
    verbose = FALSE,
    signedbyte = TRUE,
    collapse_degen = TRUE,
    raw_datavals = FALSE
  )
  
  # Get DQF using start and count arguments
  nativeGrid[["DQF"]] <- ncdf4::ncvar_get(
    nc, 
    varid = "DQF",
    start = c(start_x, start_y),
    count = c(count_x, count_y),
    verbose = FALSE,
    signedbyte = TRUE,
    collapse_degen = TRUE,
    raw_datavals = FALSE
  )
  
  label <- 
    goesaodc_convertFilenameToDatetime(files[i]) %>%
    strftime(format = "%Y%m%d%H%M", timezone = "UTC")
  
  nativeGridList[[label]] <- nativeGrid
  
} # END of timestep loop

# ----- Plot the native grid for a cheap, upside-down movie --------------------

for ( i in seq_along(files) ) {
  image(nativeGridList[[i]]$AOD)
  
}

# ----- Average together for an hour -------------------------------------------

# Create a list containing only 2-D AOD arrays
AODList <- nativeGridList %>% purrr::map(~ .x$AOD)

# Create a 3-D AOD array
stackedNativeArray <- abind::abind(AODList, rev.along = 0)

# Calculate the mean at every x-y location
AOD_mean <- apply(stackedNativeArray, c(1,2), mean, na.rm = TRUE)

# Have a look
image(AOD_mean)




