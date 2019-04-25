#' @export
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

goes_createSpatialPoints <- function(
  nc,
  xmn = NULL,
  xmx = NULL,
  ymn = NULL,
  ymx = NULL,
  dqfLevel = NULL
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  if (!is.null(dqfLevel)) {
    if (!(dqfLevel %in% c(0, 1, 2, 3))) {
      stop(paste0("dqfLevel must be NULL, 0, 1, 2, or 3"))
    }
  }
  
  # ----- Filter Data ----------------------------------------------------------
  
  # create tibble
  tbl <- goes_createTibble(nc)

  # filter tibble
  if (!is.null(xmn)) {
    tbl <- dplyr::filter(tbl, lon >= xmn)
  }
  if (!is.null(xmx)) {
    tbl <- dplyr::filter(tbl, lon <= xmx)
  }
  if (!is.null(ymn)) {
    tbl <- dplyr::filter(tbl, lat >= ymn)
  }
  if (!is.null(ymx)) {
    tbl <- dplyr::filter(tbl, lat <= ymx)
  }
  if (!is.null(dqfLevel)) {
    tbl <- dplyr::filter(tbl, DQF <= dqfLevel)
  }
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  pts <- sp::SpatialPointsDataFrame(coords = dplyr::select(tbl, c(lon, lat)),
                                    data = dplyr::select(tbl, -c(lon, lat)))
  
  return(pts)
}

# ===== Debugging ==============================================================

if (FALSE) {
  filePath <- "/Users/tom/Projects/MazamaSatelliteUtils/local_data/OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc"
  nc <- nc_open(filePath)
  pts <- goes_createSpatialPoints(nc)
}
