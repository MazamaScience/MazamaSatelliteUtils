#' @export
#' @importFrom rlang .data
#'
#' @title Create a SpatialPointsDataFrame of GOES data
#'
#' @param nc ncdf4 handle.
#' @param bbox Bounding box for the region of interest.
#' @param dqfLevel Data quality flag level.
#'
#' @description Create a SpatialPointsDataFrame of GOES data including data
#' within the specified bounding box and data quality flag level. 
#' 
#' The \code{dqfLevel} parameter can take a value of:
#'
#' \itemize{
#' \item{0}{ -- High quality retrieval flag}
#' \item{1}{ -- Medium quality retrieval flag}
#' \item{2}{ -- Low quality retrieval flag}
#' \item{3}{ -- No retrieval quality flag}
#' }
#' 
#' The \code{bbox} parameter can be a vector of floats in c(lonLo, lonHi, latLo,
#' latHi) order or the return value from \code{sp::bbox()} or 
#' \code{raster::extent()}.
#'
#' @return SpatialPointsDataFrame
#'
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' netCDF <- system.file("extdata",
#'                       "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc",
#'                       package = "MazamaSatelliteUtils")
#' nc <- goesaodc_openFile(netCDF)
#' sp <- goesaodc_createSpatialPoints(nc, dqfLevel = 2)
#' maps::map("state")
#' goesaodc_plotSpatialPoints(sp, cex = 0.3, add = TRUE)
#' }

goesaodc_createSpatialPoints <- function(
  nc,
  bbox = NULL,
  dqfLevel = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !is.null(dqfLevel) ) {
    if ( !(dqfLevel %in% c(0, 1, 2, 3)) ) {
      stop(paste0("dqfLevel must be NULL, 0, 1, 2, or 3"))
    }
  }
  
  # ----- Filter data ----------------------------------------------------------
  
  # create tibble
  tbl <- goesaodc_createTibble(nc)

  if ( !is.null(dqfLevel) ) {
    tbl <- dplyr::filter(tbl, .data$DQF <= dqfLevel)
  }
  
  if ( !is.null(bbox) ) {
    
    boundaries <- bboxToVector(bbox)
    lonLo <- boundaries[1]
    lonHi <- boundaries[2]
    latLo <- boundaries[3]
    latHi <- boundaries[4]
    
    tbl <-
      tbl %>%
      dplyr::filter(.data$lon >= lonLo) %>%
      dplyr::filter(.data$lon <= lonHi) %>%
      dplyr::filter(.data$lat >= latLo) %>%
      dplyr::filter(.data$lat <= latHi)
  }
  
  if ( nrow(tbl) == 0 )
    stop("No data for selected region")
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  spatialPoints <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
    data = dplyr::select(tbl, -c(.data$lon, .data$lat))
  )
  
  return(spatialPoints)
  
}
