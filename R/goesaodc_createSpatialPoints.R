#' @export
#' @importFrom rlang .data
#' 
#' @title Create a SpatialPointsDataFrame of GOES data
#' 
#' @param nc ncdf4 handle
#' @param xmn lower longitude extent
#' @param xmx upper longitude extent
#' @param ymn lower latitude extent
#' @param ymx upper latitude extent
#' @param dqfLevel data quality flag level
#' 
#' @description Create a SpatialPointsDataFrame of GOES data including data
#' within the specified extent and data quality flag level. Data quality level
#' can take a value of:
#' 
#' 0: High quality retrieval flag
#' 1: Medium quality retrieval flag
#' 2: Low quality retrieval flag
#' 3: No retrieval quality flag
#' 
#' 
#' @return SpatialPointsDataFrame

goesaodc_createSpatialPoints <- function(
  nc,
  # TODO: add bbox
  xmn = NULL, # TODO: change to lonLo
  xmx = NULL, # TODO: change to lonHi
  ymn = NULL, # TODO: change to latLo
  ymx = NULL, # TODO: change to latHi
  dqfLevel = NULL
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  if ( !is.null(dqfLevel) ) {
    if (!(dqfLevel %in% c(0, 1, 2, 3))) {
      stop(paste0("dqfLevel must be NULL, 0, 1, 2, or 3"))
    }
  }
  
  # ----- Filter Data ----------------------------------------------------------
  
  # create tibble
  tbl <- goesaodc_createTibble(nc)
  
  # filter tibble
  if (!is.null(xmn)) {
    tbl <- dplyr::filter(tbl, .data$lon >= xmn)
  }
  if (!is.null(xmx)) {
    tbl <- dplyr::filter(tbl, .data$lon <= xmx)
  }
  if (!is.null(ymn)) {
    tbl <- dplyr::filter(tbl, .data$lat >= ymn)
  }
  if (!is.null(ymx)) {
    tbl <- dplyr::filter(tbl, .data$lat <= ymx)
  }
  if (!is.null(dqfLevel)) {
    tbl <- dplyr::filter(tbl, .data$DQF <= dqfLevel)
  }
  
  # TODO: check if tibble is empty and if so stop
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  spatialPoints <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(tbl, c(lon, lat)),
    data = dplyr::select(tbl, -c(lon, lat))
  )
  
  return(spatialPoints)
}

# ===== Debugging ==============================================================

if (FALSE) {
  filePath <- "/Users/tom/Projects/MazamaSatelliteUtils/local_data/OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc"
  nc <- nc_open(filePath)
  spatialPoints <- goesaodc_createSpatialPoints(nc)
}
