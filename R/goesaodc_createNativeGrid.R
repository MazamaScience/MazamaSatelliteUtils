#' @export
#' 
#' @title Create a nativeGrid object
#' 
#' @param nc ncdf4 handle or a list of handles.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param verbose Logical flag to increase messages while processing data.
#' 
#' @description Creates a \emph{nativeGrid} object with a list of matrices for: 
#' longitude, latitude, AOD values, and DQF values. The arrays are defined in
#' native i, j coordinates and are thus curvilinear as opposed to rectilinear 
#' geospatial coordinates.
#' 
#' The \code{nc} parameter can be either a single netcdf handle or a list of 
#' handles. If a list of handles is provided, grid cell values for AOD and DQF 
#' will be averaged across all nc handles. This "native grid" averaging 
#' provides a simple way to convert 5-minute data into an hourly average.
#' 
#' @return List with lon, lat, AOD and DQF matrices
#
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#'
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = "2019-10-27 14:00",
#'   timezone = "America/Los_Angeles"
#' )
#'
#' files <- goesaodc_listFiles(
#'   satID = "G17",
#'   datetime = "2019-10-27 14:00",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' ncList <- list()
#' for ( file in files ) {
#'   label <- 
#'     file %>%
#'     goesaodc_convertFilenameToDatetime() %>%
#'     MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
#'   ncList[[label]] <- goesaodc_openFile(basename(file))
#' }
#'
#' # Kincade fire region
#' kincade_bbox <- c(-124, -120, 36, 39)
#' 
#' layout(matrix(seq(2)))
#' 
#' nativeGrid <- goesaodc_createNativeGrid(ncList[1], kincade_bbox)
#' image(nativeGrid$AOD[,ncol(nativeGrid$AOD):1])
#' title("Single timestep")
#' 
#' nativeGrid <- goesaodc_createNativeGrid(ncList, kincade_bbox)
#' image(nativeGrid$AOD[,ncol(nativeGrid$AOD):1])
#' title("Average of 12 timesteps")
#'
#' layout(1)
#' }

goesaodc_createNativeGrid <- function (
  nc = NULL,
  bbox = bbox_CONUS,
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(bbox)
  
  if ( "list" %in% class(nc) ) {
    ncList <- nc
  } else if ( "ncdf4" %in% class(nc) ) {
    ncList <- list(nc)
  } else {
    stop("Parameter 'nc' must be of type 'list' or 'ncdf4'")
  }
  
  # Guarantee they all refer to the same satellite
  satIDs <- sapply(
    ncList,
    function(x) { ncdf4::ncatt_get(x, varid = 0, attname = "platform_ID")$value }
  )
  
  if ( length(unique(satIDs)) > 1 )
    stop("Parameter 'nc' includes data from more than one satellite")
  
  satID <- unique(satIDs)
  
  # ----- Get grid data --------------------------------------------------------
  
  # Choose which gridFile to load based on satID
  if ( satID == "G16") {
    gridFile <- "goesEastGrid.rda"
  } else if ( satID == "G17" ) {
    gridFile <- "goesWestGrid.rda"
  }
  
  # Assemble the correct filepath based on satID and Data directory
  filePath <- file.path(getSatelliteDataDir(), gridFile)
  
  # Test for grid existence and if found, load it.
  if ( file.exists(filePath) ) {
    goesGrid <- get(load(filePath))
  } else {
    stop("Grid file not found. Run 'installGoesGrids()' first")
  }  
  
  # ----- Create a grid mask ---------------------------------------------------
  
  bbox <- bboxToVector(bbox)
  
  lonLo <- bbox[1]
  lonHi <- bbox[2]
  latLo <- bbox[3]
  latHi <- bbox[4]
  
  # Matrices of the same dimensions as AOD and DQF
  lonMatrix <- goesGrid$lon
  latMatrix <- goesGrid$lat
  
  # Create a matrix of logicals identifying grid cells within bbox
  gridMask <-
    lonMatrix >= lonLo &
    lonMatrix <= lonHi &
    latMatrix >= latLo &
    latMatrix <= latHi
  
  gridMask[is.na(gridMask)] <- FALSE
  
  # ----- Find the i,j bounding box in curvilinear grid space ------------------
  
  # Suppress "no non-missing arguments" warnings
  suppressWarnings({
    
    # Find the first row in each column inside the bbox
    iLos <- apply(gridMask, 2, function(x) { min(which(x), na.rm = TRUE) })
    iLo <- min(iLos) # lots of Inf but that's OK
    
    # Last row
    iHis <- apply(gridMask, 2, function(x) { max(which(x), na.rm = TRUE) })
    iHi <- max(iHis) # lots of -Inf but that's OK
    
    # First column
    jLos <- apply(gridMask, 1, function(x) { min(which(x), na.rm = TRUE) })
    jLo <- min(jLos) # lots of Inf but that's OK
    
    # Last column
    jHis <- apply(gridMask, 1, function(x) { max(which(x), na.rm = TRUE) })
    jHi <- max(jHis) # lots of -Inf but that's OK
    
  })
  
  # Convert to the variables we pass to ncvar_get()
  start_x <- iLo
  count_x <- iHi - iLo + 1
  
  start_y <- jLo
  count_y <- jHi - jLo + 1
  
  # ----- Create a list of 'nativeGrid' objects --------------------------------
  
  nativeGridList <- list()
  
  for ( nc in ncList ) {
    
    # ----- Create a 'nativeGrid' object ---------------------------------------
    
    nativeGrid <- list()
    
    # Get subset lons and lats from the original grid file
    nativeGrid[["lon"]] <- lonMatrix[iLo:iHi,jLo:jHi]
    nativeGrid[["lat"]] <- latMatrix[iLo:iHi,jLo:jHi]
    
    # Get AOD using start and count arguments
    raw_aod_data <- ncdf4::ncvar_get(
      nc, 
      varid = "AOD",
      start = c(start_x, start_y),
      count = c(count_x, count_y),
      verbose = FALSE,
      signedbyte = FALSE,
      collapse_degen = TRUE,
      raw_datavals = TRUE
    )
    
    # Get AOD attributes
    aod_attributes <- ncdf4::ncatt_get(nc, "AOD")
    
    # NOTE:  As of ncdf4 version 1.16.1, the signedbyte flag is only used to
    # NOTE:  interpret singlye byte values. GOES AODC values are stored as
    # NOTE:  unsigned short ints but ncdf4::ncvar_get interprets these 16 bits
    # NOTE:  as signed short ints. Hence the need for conversion.
    
    # Convert the AOD data from unsigned short int and apply scaling
    nativeGrid[["AOD"]] <- goesaodc_scaleAOD(raw_aod_data, aod_attributes)
    
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
    
    label <- ncdf4::ncatt_get(nc, varid = 0)$time_coverage_start
    
    nativeGridList[[label]] <- nativeGrid
    
  }
  
  # DEBUGGING
  # Plot the native grid for a cheap movie (note that we need to reverse j)
  
  if ( FALSE ) {
    
    for ( nativeGrid in nativeGridList ) {
      image(nativeGrid$AOD[,ncol(nativeGrid$AOD):1])
    }
    
  }
  
  # ----- Calculate the average AOD and DQF ------------------------------------
  
  nativeGrid <- nativeGridList[[1]]
  
  if ( length(nativeGridList) > 1 ) {
    
    # Create lists containing only 2-D arrays
    AODList <- nativeGridList %>% purrr::map(~ .x$AOD)
    DQFList <- nativeGridList %>% purrr::map(~ .x$DQF)
    
    # Create 3-D arrays
    stackedAODArray <- abind::abind(AODList, rev.along = 0)
    stackedDQFArray <- abind::abind(DQFList, rev.along = 0)
    
    # Calculate the mean at every x-y location
    AOD_mean <- apply(stackedAODArray, c(1,2), mean, na.rm = TRUE)
    DQF_mean <- apply(stackedDQFArray, c(1,2), mean, na.rm = TRUE)
    
    nativeGrid$AOD <- AOD_mean
    nativeGrid$DQF <- DQF_mean
    
  }
  
  # DEBUGGING
  if ( FALSE ) {
    image(nativeGrid$AOD[,ncol(nativeGrid$AOD):1])
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(nativeGrid)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(MazamaSpatialUtils)
  setSpatialDataDir("~/Data/Spatial")
  loadSpatialData("USCensusStates")
  loadSpatialData("USCensusCounties")
  
  library(MazamaSatelliteUtils)
  setSatelliteDataDir("~/Data/Satellite")
  
  # nc file
  #files <- goesaodc_downloadAOD("G17", 2019102714, timezone = "America/Los_Angeles")
  files <- goesaodc_listFiles("G17", "2019-10-27 14:00", timezone = "America/Los_Angeles")
  
  ncList <- list()
  for ( file in files ) {
    label <- 
      file %>%
      goesaodc_convertFilenameToDatetime() %>%
      MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
    ncList[[label]] <- goesaodc_openFile(basename(file))
  }
  
  
  # bbox
  ca <- subset(USCensusStates, stateCode == "CA")
  bbox <- sp::bbox(ca) %>% bboxToVector()
  
  verbose <- TRUE
  
  nativeGrid <- goesaodc_createNativeGrid(ncList, bbox, verbose = FALSE)
  
  image(nativeGrid$AOD[,ncol(nativeGrid$AOD):1])
  
}