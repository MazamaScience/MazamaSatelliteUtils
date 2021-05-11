#' @export
#' 
#' @title Calculate the average of a series of GOES scan points
#' 
#' @description Creates a \code{SpatialPointsDataFrame} of averaged AOD 
#' readings from a \code{list} of GOES scan \code{SpatialPointsDataFrame}s.
#' 
#' @param spdfList A \code{list} of GOES scan \code{SpatialPointsDataFrame}s.
#' @param na.rm Logical flag whether to remove NA values before calculating the
#' average. Defaults to \code{FALSE}.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bboxOregon <- c(-125, -116, 42, 47)
#' 
#' scanFiles <- goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' scanPointsList <- goesaodc_createScanPoints(
#'   filename = scanFiles,
#'   bbox = bboxOregon
#' )
#' 
#' goesaodc_calcAverageScanPoints(
#'   spdfList = scanPointsList,
#'   na.rm = FALSE
#' )
#' }

goesaodc_calcAverageScanPoints <- function(
  spdfList = NULL,
  na.rm = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !("list" %in% class(spdfList)) )
    stop("Parameter 'spdfList' must be a list of 'SpatialPointsDataFrame' objects")
  
  if ( length(spdfList) == 0 )
    stop("Parameter 'spdfList' must contain at least one element")
  
  for ( spdf in spdfList ) {
    if ( !("SpatialPointsDataFrame" %in% class(spdf)) )
      stop("All elements in parameter 'spdfList' must be 'SpatialPointsDataFrame' objects")
  }
  
  # ----- Calculate average point AOD values -----------------------------------
  
  aodVectors <- lapply(spdfList, function(scanPoints) {
    tibble::as_tibble(scanPoints)$AOD
  })
  
  avgAodValues <-
    aodVectors %>%
    dplyr::bind_cols() %>%
    dplyr::rowwise() %>%
    rowMeans(na.rm = na.rm)
  
  avgSpdf <- sp::SpatialPointsDataFrame(
    coords = spdfList[[1]]@coords,
    data = data.frame(
      AOD = avgAodValues
    )
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(avgSpdf)
  
}
