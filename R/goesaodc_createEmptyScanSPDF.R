#' @title Create an empty GOES scan SPDF
#' 
#' @description Creates a SpatialPointsDataFrame of AOD readings from one or 
#' more GOES scans. Scans are specified either by filename or satellite + time 
#' information.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param filename Name of a scan file.
#' @param bbox Bounding box for the region of interest. Defaults to CONUS.

goesaodc_createEmptyScanSPDF <- function(
  satID = NULL,
  filename = NULL,
  bbox = bbox_CONUS
) {
  
  # ----- Get satellite grid ---------------------------------------------------
  
  # If a filename was given, extract the satellite ID from it
  if ( is.null(satID) ) {
    MazamaCoreUtils::stopIfNull(filename)
    filePattern <- "OR_ABI-L2-AODC-M[0-9]_(G16|G17)_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
    satID <- stringr::str_match(filename, filePattern)[1,2]
  }
  
  # Get the grid file for the requested satellite
  if ( toupper(satID) == "G16") {
    gridFile <- "goesEastGrid.rda"
  } else if ( toupper(satID) == "G17" ) {
    gridFile <- "goesWestGrid.rda"
  } else {
    stop("Parameter 'satID' must be either 'G16' or 'G17'")
  }
  
  # Load the satellite grid
  gridFilePath <- file.path(getSatelliteDataDir(), gridFile)
  if ( file.exists(gridFilePath) ) {
    satGrid <- get(load(gridFilePath))
  } else {
    stop(paste0("Grid file '", gridFilePath, "'not found. Run 
    'installGoesGrids()' first."))
  }
  
  # ----- Create a grid mask ---------------------------------------------------
  
  bbox <- bboxToVector(bbox)
  
  lonLo <- bbox[1]
  lonHi <- bbox[2]
  latLo <- bbox[3]
  latHi <- bbox[4]
  
  # Matrices of the same dimensions as AOD and DQF
  lonMatrix <- satGrid$lon
  latMatrix <- satGrid$lat
  
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
  
  nativeGrid <- list()
  
  # Get subset lons and lats from the original grid file
  nativeGrid[["lon"]] <- lonMatrix[iLo:iHi,jLo:jHi]
  nativeGrid[["lat"]] <- latMatrix[iLo:iHi,jLo:jHi]
  
  nativeGrid[["AOD"]] <- matrix(
    data = rep(NA, nrow(nativeGrid[["lon"]]) * ncol(nativeGrid[["lon"]])),
    nrow = nrow(nativeGrid[["lon"]]),
    ncol = ncol(nativeGrid[["lon"]])
  )
  
  nativeGrid[["DQF"]] <- matrix(
    data = rep(NA, nrow(nativeGrid[["lon"]]) * ncol(nativeGrid[["lon"]])),
    nrow = nrow(nativeGrid[["lon"]]),
    ncol = ncol(nativeGrid[["lon"]])
  )
  
  # ----- Create tibble --------------------------------------------------------
  
  # Build the tibble
  varList <- list()
  varList[["lon"]] <- as.numeric(nativeGrid$lon)
  varList[["lat"]] <- as.numeric(nativeGrid$lat)
  varList[["AOD"]] <- as.numeric(nativeGrid$AOD)
  varList[["DQF"]] <- as.numeric(nativeGrid$DQF)
  
  tbl <- tibble::as_tibble(varList)
  
  # Guarantee W, E, S, N order
  bbox <- bboxToVector(bbox)
  
  # Crops readings to be within the bbox
  tbl <-
    tbl %>%
    dplyr::filter(
      .data$lon >= bbox[1] &
      .data$lon <= bbox[2] &
      .data$lat >= bbox[3] &
      .data$lat <= bbox[4]
    )
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  spdf <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
    data = dplyr::select(tbl, -c(.data$lon, .data$lat))
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(spdf)
  
}


if ( FALSE ) {

  library(MazamaSatelliteUtils)
  setSatelliteDataDir("~/Data/Satellite")
 
  goesaodc_createScanEmptySPDF(satID = "G16")
  goesaodc_createScanEmptySPDF(satID = "G17")
  goesaodc_createScanEmptySPDF(filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc")
  
}

