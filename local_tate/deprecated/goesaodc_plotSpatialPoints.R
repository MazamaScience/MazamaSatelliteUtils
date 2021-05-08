#' @export
#' 
#' @title Create a quick plot of a GOES AOD SpatialPointsDataFrame
#' 
#' @description Quickly subsamples and plots points in a GOES AOD 
#' spatialPointsDataFrame
#' 
#' @param spatialPoints SpatialPointsDataFrame.
#' @param var GOES data variable ("AOD, "DQF" or "ID"); Defaults to "AOD".
#' @param n Sample size.
#' @param colBins Number of color bins.
#' @param breaks Vector of color breaks.
#' @param pch Index of plot character (0-25).
#' @param cex Plot symbol scale factor.
#' @param paletteName RColorBrewer palette name.
#' @param add Logical specifying whether to create a new plot or add to an 
#' existing one.
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' netCDF <- goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = "2019-10-27 10", 
#'   timezone = "America/Los_Angeles" 
#' )[1]
#' 
#' # Kincade fire region
#' kincade_bbox <- c(-124, -120, 36, 39)
#'
#' nc <- goesaodc_openFile(netCDF)
#' sp <- goesaodc_createSpatialPoints(nc, bbox = kincade_bbox, dqfLevel = 3)
#' 
#' goesaodc_plotSpatialPoints(sp, cex = 0.5)
#' maps::map(
#'   database = "state",
#'   regions = "california",
#'   xlim = c(-125, -119),
#'   ylim = c(35, 40),
#'   add  = TRUE
#' )
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
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(spatialPoints)
  
  if ( !(var %in% c("AOD", "DQF", "ID")) ) {
    stop("Parameter 'var' must be either 'AOD', 'DQF', or 'ID'")
  }
  
  if ( !is.numeric(n) || n < 0 ) {
    stop("Parameter 'n' must be a number > 0")
  }
  
  if ( !is.numeric(colBins) || colBins <= 0 ) {
    stop("Parameter 'colBins' must be a number > 0")
  }
  
  # TODO: Validate 'breaks' param
  
  if ( !is.character(paletteName) ) {
    stop("Parameter 'paletteName' must be an RColorBrewer palette name")
  }
  
  if ( !is.numeric(pch) ) {
    stop("Parameter 'pch' must be an integer from 0 to 25")
  }
  
  if ( !is.numeric(cex) ) {
    stop("Parameter 'cex' must be number > 0")
  }
  
  if ( !is.logical(add) ) {
    stop("Parameter 'add' must be either TRUE or FALSE")
  }
  
  # ----- Subsample points -----------------------------------------------------
  
  if ( n < nrow(spatialPoints) ) {
    indices <- sample(seq_len(nrow(spatialPoints)), n)
    spatialPointsSub <- spatialPoints[indices,]
  } else {
    spatialPointsSub <- spatialPoints
  }
  
  # ----- Define palette -------------------------------------------------------
  
  # Make breaks for specified number of equally sized color bins
  # TODO: Use quantiles like quantile(aodValues, seq(from = 0.00, to = 1.00, by = 0.10), na.rm = TRUE)
  if ( is.null(breaks) ) {
    mn <- min(spatialPointsSub[[var]], na.rm = TRUE)
    mx <- max(spatialPointsSub[[var]], na.rm = TRUE)
    range <- mx - mn
    
    breaks <- c(mn)
    for (i in 1:colBins) {
      breaks <- c(breaks, mn + i*(range/colBins))
    }
  }
  
  cols <- RColorBrewer::brewer.pal(length(breaks)-1, paletteName)
  col_i <- .bincode(spatialPointsSub[[var]], breaks)
  col_v <- cols[col_i]
  
  # ----- Plot points ----------------------------------------------------------
  
  plot(
    spatialPointsSub,
    pch = pch,
    col = col_v,
    cex = cex,
    add = add
  )
  
}
