#' @export
#' 
#' @importFrom rlang .data
#' 
#' @title Create a tibble from a ncdf4 handle
#' 
#' @description Creates a tibble from a netCDF file with columns: AOD, DQF, lon 
#' and lat. This information is sufficient to plot as points or create a raster 
#' object.
#' 
#' @param nc ncdf4 handle or a list of handles.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Allowed data quality level. All readings with a DQF value
#' above this level will have their AOD values set to NA. Must be either 0, 1, 
#' 2, or 3, with 0 being the highest quality. Defaults to 3.
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
#' netCDF <- goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = "2019-10-27 10", 
#'   timezone = "America/Los_Angeles" 
#' )[1]
#'
#' nc <- goesaodc_openFile(netCDF)
#'
#' tbl <- goesaodc_createTibble(nc)
#' head(tbl)
#' 
#' # Tibble based on bbox filtered region
#' 
#' # Kincade fire region
#' bbox <- c(-124, -120, 36, 39)
#' 
#' filtered_tbl <- goesaodc_createTibble(nc, bbox)
#' head(filtered_tbl)
#' }

goesaodc_createTibble <- function(
  nc = NULL, 
  bbox = bbox_CONUS,
  dqfLevel = 3
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(bbox)
  
  if ( !(dqfLevel %in% c(0, 1, 2, 3)) ) {
    stop(paste0("Parameter 'dqfLevel' must be 0, 1, 2, or 3"))
  }
  
  # Check that nc has GOES projection
  if ( !goesaodc_isGoesProjection(nc) ) {
    stop("Parameter 'nc' does not have standard GOES-R projection information.")
  }

  if ( "list" %in% class(nc) ) {
    ncList <- nc
  } else if ( "ncdf4" %in% class(nc) ) {
    ncList <- list(nc)
  } else {
    stop("Parameter 'nc' must be of type 'list' or 'ncdf4'")
  }
  
  # ----- Assemble the tibble --------------------------------------------------
  
  # Get data on the native satellite grid
  # NOTE:  If nc is a list, AOD and DQF will represent an average
  nativeGrid <- goesaodc_createNativeGrid(ncList, bbox)
  
  # Build the tibble
  varList <- list()
  varList[["lon"]] <- as.numeric(nativeGrid$lon)
  varList[["lat"]] <- as.numeric(nativeGrid$lat)
  varList[["AOD"]] <- as.numeric(nativeGrid$AOD)
  varList[["DQF"]] <- as.numeric(nativeGrid$DQF)
  
  tbl <- tibble::as_tibble(varList)
  
  # NOTE:  Using the bounding box during nativeGrid ingest results in extra
  # NOTE:  records because the nativeGrid is curvilinear. We limit the records
  # NOTE:  to the requested bbox here.
  
  # Guarantee W, E, S, N order
  bbox <- bboxToVector(bbox)
  
  tbl <-
    tbl %>%
    dplyr::filter(
      .data$lon >= bbox[1] &
      .data$lon <= bbox[2] &
      .data$lat >= bbox[3] &
      .data$lat <= bbox[4]
    )
  
  # Convert AOD readings to NA for points outside the DQF threshold
  # TODO: Can this be done in a cleaner way with dplyr mutate()?
  tbl[tbl$DQF > dqfLevel, which(colnames(tbl) == "AOD")] <- NA
    
  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
  
}

