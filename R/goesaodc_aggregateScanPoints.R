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
#' goesaodc_aggregateScanPoints(scanPointsList)
#' }

goesaodc_aggregateScanPoints <- function(
  spdfList = NULL,
  fun = mean
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
    coords = scanPointsList[[1]]@coords,
    data = data.frame(
      AOD = rep(NA, dim(scanPointsList[[1]])[1])
    )
  )
  
  aodValues <- rep(NA, length(scanPointsList))
  
  for ( i in 1:dim(scanPointsList[[1]])[1] ) {
    
    for ( j in 1:length(scanPointsList) ) {
      spdf <- scanPointsList[[j]]
      aodValues[j] <- spdf$AOD[i]
    }
    
    fun <- mean
    aggregateSpdf$AOD[i] <- mean(aodValues)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(aggregateSpdf)
  
}
