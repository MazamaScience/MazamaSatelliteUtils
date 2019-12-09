#' @export
#' @importFrom rlang .data
#' 
#' @title Create a tibble from ncdf4 handle
#' 
#' @param nc ncdf4 handle or a list of handles.
#' @param bbox Geographic extent of area of interest; Defaults to CONUS.
#' @param verbose Logical flag to increase messages while processing data.
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
#' ncFile <- goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = "2019-10-27 10", 
#'   timezone = "America/Los_Angeles" 
#'   )[1]
#'
#' nc <- goesaodc_openFile(ncFile)
#'
#' tbl <- goesaodc_createTibble(nc)
#' head(tbl)
#' 
#' # Tibble based on BBOX filtered extent of tibble
#' library(MazamaSatelliteUtils)
#' 
#' ncFile <- goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = "2019-10-27 10", 
#'   timezone = "America/Los_Angeles" 
#'   )[1]
#'
#' nc <- goesaodc_openFile(ncFile)
#' 
#' # 2019 Kincade fire in Sonoma county
#' bbox <- c(-124, -120, 36, 39)
#' 
#' filtered_tbl <- goesaodc_createTibble(nc, bbox)
#' head(filtered_tbl)
#' 
#' }

goesaodc_createTibble <- function(
  nc = NULL, 
  bbox = bbox_CONUS,
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(bbox)
  
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
  
  # Get data on the native, satellite grid
  # NOTE:  If nc is a list, AOD and DQF will represent an average
  nativeGrid <- goesaodc_createNativeGrid(ncList, bbox, verbose)
  
  # Build the tibble
  varList <- list()
  varList[["lon"]] <- as.numeric(nativeGrid$lon)
  varList[["lat"]] <- as.numeric(nativeGrid$lat)
  varList[["AOD"]] <- as.numeric(nativeGrid$AOD)
  varList[["DQF"]] <- as.numeric(nativeGrid$DQF)
  
  # NOTE:  Use tidyr::drop_na() to drop records with ANY missing values
  tbl <-
    tibble::as_tibble(varList) %>%
    tidyr::drop_na()
  
  # NOTE:  Using the bounding box during nativeGrid ingest results in extra
  # NOTE:  records because the nativeGrid is curvilinear. We limit the records
  # NOTE:  to the requested bbox here.
  
  # Guarantee W, E, S, N order
  bbox <- bboxToVector(bbox)
  tbl <-
    tbl %>%
    dplyr::filter(.data$lon >= bbox[1] &
                    .data$lon <= bbox[2] &
                    .data$lat >= bbox[3] &
                    .data$lat <= bbox[4])
    
  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
  
}

