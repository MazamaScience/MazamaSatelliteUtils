#' @export
#' 
#' @title Creates list of tibbles from a list of .nc files
#' 
#' @param ncFiles ncdf4 handle
#' @param bbox geographic extents to limit data ingestion from NetCDF files to
#' 
#' @description Create a list of tibbles with columns: AOD, DQF, lon and lat.
#' This information is sufficient to plot as points or create a raster object.
#' 
#' @return list of Tibbles (dataframe) with NetCDF variables and associated 
#' locations. List names correspond to timestamps of files in input list.
#
#' @examples
#' \donttest{
#' # Process full extent of Gridfile
#' library(MazamaSatelliteUtils)
#'
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' goesaodc_downloadAOD(
#'   satID = "G16", 
#'   datetime = "201930019",
#'   endtime = "2019300211", 
#'   timezone = "UTC", 
#'   isJulian = TRUE)
#'
#' my_files <- c("OR_ABI-L2-AODC-M6_G16_s20193001901344_e20193001904117_c20193001907158.nc")
#' 
#' tbl_list <- goesaodc_createNativeGrid(my_files)
#' head(tbl_list)
#' 
#' # Tibble based on BBOX filtered extent of tibble
#' library(MazamaSatelliteUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' my_files <- c("OR_ABI-L2-AODC-M6_G16_s20193001901344_e20193001904117_c20193001907158.nc",
#'               "OR_ABI-L2-AODC-M6_G17_s20193002116196_e20193002118569_c20193002121014.nc")
#'
#' kincade_bbox <- c(-124, -120, 36, 39)
#' 
#' filtered_list <- goesaodc_createNativeGrid(
#'   my_files,
#'   bbox = kincade_bbox)
#'   
#' head(filtered_list)
#' 
#' }

goesaodc_createNativeGrid <- function (
  nc_files = NULL,
  bbox = NULL
) {

  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc_files)
  
  tibbleList <- list()
  
  for ( i in seq_along(nc_files) ) {
    
    # Create varList to store values extracted from .nc file
    varList <- list()
    
    print(paste0("Working on ", nc_files[i], " ..."))
    nc <- goesaodc_openFile(nc_files[i])
    
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
    
    # ---- Create Tibble ---------------------------------------------------
    
    # Build full extent Tibble
    if ( is.null(bbox) ) { 
      
      # Get lon and lat from grid file
      varList[["lon"]] <- as.numeric( goesGrid$longitude )
      varList[["lat"]] <- as.numeric( goesGrid$latitude )
      
      # Get AOD and DQF from netCDF
      varList[["AOD"]] <- as.numeric(ncdf4::ncvar_get(nc, "AOD"))
      varList[["DQF"]] <- as.numeric(ncdf4::ncvar_get(nc, "DQF"))
      
    } else {
      
      # Build filtered tibble based on BBOX coordinates
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
    }
    
    ncdf4::nc_close(nc) # Close the .nc filehandle
    
    # Create a label from file's timestamp
    label <-
      goesaodc_convertFilenameToDatetime(nc_files[i]) %>%
      strftime(format = "%Y%m%d%H%M", timezone = "UTC")
    
    # Filter out the na's if full extent (too restrictive)
    if ( is.null(bbox) ) {
      tbl <-
        tibble::as_tibble(varList) %>% tidyr::drop_na()
    } else {
      tbl <- tibble::as_tibble(varList)
    }
    
    # Store the tibble in the tibbleList with a timestamp as label/key
    tibbleList[[label]] <- tbl 
    
  }
  return(tibbleList)
  
}
