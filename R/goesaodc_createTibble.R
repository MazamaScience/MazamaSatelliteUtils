#' @export
#' 
#' @title Create a tibble from ncdf4 handle
#' 
#' @param nc ncdf4 handle
#' @param bbox geographic extents to limit data ingestion from NetCDF
#' 
#' @description Create a tibble with columns: AOD, DQF, lon and lat.
#' This information is sufficient to plot as points or create a raster object.
#' 
#' @return Tibble (dataframe) with NetCDF variables and associated locations.
#
#' @examples
#' \donttest{
#' # Tibble based on full extent of Gridfile
#' library(MazamaSatelliteUtils)
#'
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' ncFile <- system.file(
#'   "extdata", 
#'   "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc", 
#'   package = "MazamaSatelliteUtils"
#' )
#'                      
#' nc <- goesaodc_openFile(ncFile)
#'
#' tbl <- goesaodc_createTibble(nc)
#' head(tbl)
#' 
#' # Tibble based on BBOX filtered extent of tibble
#' library(MazamaSatelliteUtils)
#' 
#' ncFile <- system.file(
#'   "extdata", 
#'   "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc", 
#'   package = "MazamaSatelliteUtils"
#' )
#'
#' nc <- goesaodc_openFile(ncFile)
#' 
#' kincade_bbox <- c(-124, -120, 36, 39)
#' 
#' filtered_tbl <- goesaodc_createTibble(
#'   nc,
#'   bbox = kincade_bbox
#' )
#' 
#' }

goesaodc_createTibble <- function(
  nc = NULL, 
  bbox = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  
  # Check that nc has GOES projection
  if ( !goesaodc_isGoesProjection(nc) ) {
    stop("Parameter 'nc' does not have standard GOES-R projection information.")
  }
  
  # ----- Get grid data --------------------------------------------------------
  
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
  
  
  # ---- Create varList to store values needed to create Tibble ----------------
  varList <- list()
  
  # ---- If BBOX present, filter based on its extents --------------------------
  
  if ( !is.null(bbox) ) {
    bbox <- bboxToVector(bbox)
    
    lonLo <- bbox[1]
    lonHi <- bbox[2]
    latLo <- bbox[3]
    latHi <- bbox[4]
    
    # Matrices of the same dimensions as AOD and DQF
    lonMatrix <- goesGrid$longitude
    latMatrix <- goesGrid$latitude
    
    # Create a matrix of logicals identifying grid cells within bbox_oregon
    gridMask <-
      lonMatrix >= lonLo &
      lonMatrix <= lonHi &
      latMatrix >= latLo &
      latMatrix <= latHi
    
    gridMask[is.na(gridMask)] <- FALSE
    
    suppressWarnings({
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
    })
    
    # Convert to the variables we pass to ncvar_get()
    start_x <- iLo
    count_x <- iHi - iLo + 1
    
    start_y <- jLo
    count_y <- jHi - jLo + 1
    
    # Get subset lons and lats from the original grid file
    varList[["lon"]] <- as.numeric( lonMatrix[iLo:iHi,jLo:jHi] )
    varList[["lat"]] <- as.numeric( latMatrix[iLo:iHi,jLo:jHi] )
    
    # Get AOD using start and count arguments
    varList[["AOD"]] <- as.numeric(ncdf4::ncvar_get(
      nc,
      varid = "AOD",
      start = c(start_x, start_y),
      count = c(count_x, count_y),
      verbose = FALSE,
      signedbyte = TRUE,
      collapse_degen = TRUE,
      raw_datavals = FALSE
    ))
    
    # Get DQF using start and count arguments
    varList[["DQF"]] <- as.numeric(ncdf4::ncvar_get(
      nc,
      varid = "DQF",
      start = c(start_x, start_y),
      count = c(count_x, count_y),
      verbose = FALSE,
      signedbyte = TRUE,
      collapse_degen = TRUE,
      raw_datavals = FALSE
    ))
  } else { # ---- Original, unfiltered code ------------------------------------
  
  # Get lon and lat from grid file
  varList[["lon"]] <- as.numeric( goesGrid$longitude )
  varList[["lat"]] <- as.numeric( goesGrid$latitude )
  
  # Get AOD and DQF from netCDF
  varList[["AOD"]] <- as.numeric(ncdf4::ncvar_get(nc, "AOD"))
  varList[["DQF"]] <- as.numeric(ncdf4::ncvar_get(nc, "DQF"))
  }
  
  # Create a tibble with all columns but removing rows if any of the columns
  # are missing.
  # TODO:  tidyr::drop_na() may be too restrictive for multiple data columns.
  tbl <-
    tibble::as_tibble(varList) %>%
    tidyr::drop_na()
  
  return(tbl)
  
}

