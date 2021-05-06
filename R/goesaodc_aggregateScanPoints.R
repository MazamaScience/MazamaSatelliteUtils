#' @export
#' 
#' @title Aggregate GOES scan points
#' 
#' @description Creates a \code{SpatialPointsDataFrame} of aggregated AOD 
#' readings from a \code{list} of GOES scan \code{SpatialPointsDataFrame}s.
#' 
#' @param spdfList A \code{list} of GOES scan \code{SpatialPointsDataFrame}s.
#' @param fun The function to use for aggregating AOD values. Defaults to 
#' \code{mean}.
#' @param na.rm Logical flag whether to remove NA values before performing the
#' \code{fun} function. Defaults to \code{FALSE}.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bboxOregon <- c(-125, -116, 42, 47)
#' 
#' scanPointsList <- goesaodc_createScanPoints(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles",
#'   bbox = bboxOregon
#' )
#' 
#' goesaodc_aggregateScanPoints(
#'   spdfList = scanPointsList,
#'   na.rm = TRUE
#' )
#' }

goesaodc_aggregateScanPoints <- function(
  spdfList = NULL,
  fun = mean,
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
  
  # ----- Aggregate point AOD values -------------------------------------------
  
  aggregateSpdf <- sp::SpatialPointsDataFrame(
    coords = spdfList[[1]]@coords,
    data = data.frame(
      AOD = rep(NA, dim(spdfList[[1]])[1])
    )
  )
  
  aodValues <- rep(NA, length(spdfList))
  
  for ( i in 1:dim(spdfList[[1]])[1] ) {
    
    for ( j in 1:length(spdfList) ) {
      spdf <- spdfList[[j]]
      aodValues[j] <- spdf$AOD[i]
    }
    
    aggregateSpdf$AOD[i] <- do.call(fun, list(aodValues, na.rm = na.rm))
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(aggregateSpdf)
  
}
