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
#' \dontrun{
#' setSatelliteDataDir("~/Data/Satellite")
#' nc <- goesaodc_openFile("OR_ABI-L2-AODC-M6_G16_s20191291201274_e20191291204047_c20191291210009.nc")
#' 
#' maps::map("state")
#' sp <- goesaodc_createSpatialPoints(nc, dqfLevel = 1)
#' goesaodc_plotSpatialPoints(sp, cex = 0.25, add = TRUE)
#' }

goesaodc_plotSpatialPoints <- function(
  spatialPoints,
  var = "AOD",
  n = 1e5,
  colBins = 5,
  breaks = NULL,
  pch = 15,
  cex = 0.5,
  paletteName = "YlOrRd",
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
  # TODO: Use quantiles
  if (is.null(breaks)) {
    mn <- min(spatialPointsSub[[var]])
    mx <- max(spatialPointsSub[[var]])
    range <- mx - mn
    
    breaks <- c(mn)
    for (i in 1:colBins) {
      breaks <- c(breaks, mn + i*(range/colBins))
    }
  }
  
  cols <- RColorBrewer::brewer.pal(length(breaks)-1, "YlOrRd")
  col_i <- .bincode(spatialPointsSub[[var]], breaks)
  col_v <- cols[col_i]
  plot(spatialPointsSub, pch=pch, col=col_v, cex=cex, add = add)
}
