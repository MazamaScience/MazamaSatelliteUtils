#' @export
#' 
#' @title Create a quick plot of a GOES AOD SpatialPointsDataFrame
#' 
#' @param spatialPoints SpatialPointsDataFrame
#' @param var Variable to plot
#' @param n Sample size
#' @param colBins number of color bins
#' @param breaks vector of color breaks
#' @param pch plot character
#' @param cex plot symbol scale factor
#' @param paletteName RColorBrewer palette name
#' @param add logical specifying whether to create a new plot or add to existing
#' 
#' @description Quickly subsample and plot points in a GOES AOD 
#' spatialPointsDataFrame
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

goesaodc_plotSpatialPoints <- function(
  spatialPoints,
  var = "AOD",
  n = 1e5,
  colBins = 5,
  breaks = NULL,
  paletteName = "YlOrRd",
  pch = 15,
  cex = 0.5,
  add = FALSE
  # TODO: color reverse?
) {
  
  # Subsample points
  if ( n < nrow(spatialPoints) ) {
    indices <- sample(seq_len(nrow(spatialPoints)), n)
    spatialPointsSub <- spatialPoints[indices,]
  } else {
    spatialPointsSub <- spatialPoints
  }
  
  # Make breaks for specifed number of equally sized color bins
  # TODO: Use quantiles like quantile(aodValues, seq(from = 0.00, to = 1.00, by = 0.10), na.rm = TRUE)
  if (is.null(breaks)) {
    mn <- min(spatialPointsSub[[var]])
    mx <- max(spatialPointsSub[[var]])
    range <- mx - mn
    
    breaks <- c(mn)
    for (i in 1:colBins) {
      breaks <- c(breaks, mn + i*(range/colBins))
    }
  }
  
  cols <- RColorBrewer::brewer.pal(length(breaks)-1, paletteName)
  col_i <- .bincode(spatialPointsSub[[var]], breaks)
  col_v <- cols[col_i]
  plot(spatialPointsSub, pch=pch, col=col_v, cex=cex, add = add)
}
