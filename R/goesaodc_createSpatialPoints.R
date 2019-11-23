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
#'
#' goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = "2019-10-27 10", 
#'   timezone = "America/Los_Angeles" 
#'   )
#'   
#' kincade_bbox <- c(-126, -119, 35, 40)   
#'
#' netCDF <- goesaodc_listFiles(
#'   satID = "G17", 
#'   datetime = "2019-10-27 10", 
#'   timezone = "America/Los_Angeles")[1]
#'   
#' nc <- goesaodc_openFile(netCDF)
#' sp <- goesaodc_createSpatialPoints(nc, dqfLevel = 3, bbox = kincade_bbox)
#' maps::map(database = "state", "regions" = c("california"), xlim = c(-126, -113.5))
#' goesaodc_plotSpatialPoints(sp, cex = 0.2, add = TRUE)
#' }

goesaodc_createSpatialPoints <- function(
  nc = NULL,
  bbox = NULL,
  dqfLevel = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(dqfLevel)
  
  if ( !is.null(dqfLevel) ) {
    if ( !(dqfLevel %in% c(0, 1, 2, 3)) ) {
      stop(paste0("dqfLevel must be NULL, 0, 1, 2, or 3"))
    }
  }
  
  # ----- Filter data ----------------------------------------------------------
  
  # Create tibble
  tbl <- goesaodc_createTibble(nc, bbox)

  # Filter based on DQF
  if ( !is.null(dqfLevel) ) {
    tbl <- dplyr::filter(tbl, .data$DQF <= dqfLevel)
  }
  
  # Filter based on bbox
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
  
  # Sanity check
  if ( nrow(tbl) == 0 )
    stop("No data for selected region")
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  spatialPoints <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
    data = dplyr::select(tbl, -c(.data$lon, .data$lat))
  )
  
  return(spatialPoints)
  
}
