#' @export
#' @title Create Side By Side Maps of a MAIAC Swath and a ws_grid Object
#' @param left_raster raster for left plot
#' @param right_raster raster for right plot
#' @param left_title title for left plot
#' @param right_title title for right plot
#' @param left_colors colors for left plot
#' @param right_colors colors for right plot
#' @param left_breaks breaks for left plot
#' @param right_breaks breaks for right plot
#' @description Create side by side maps of a MAIAC swath and a ws_grid object
#' @examples 
#' \dontrun{
#' load('~/Projects/airfire-nasa-maiac/localData/Sand_bluesky_CANSAC_6km.RData')
#' ws_grid <- Sand_bluesky_CANSAC_6km
#' maiac <- loadRawMaiac('~/Projects/airfire-nasa-maiac/localData/maiac_sands_AQUA_20160721.nc')
#' rasterList <- harmonizeSwathAndGrid(maiac, Sand_bluesky_CANSAC_6km)
#' rasterMap_sideBySide(rasterList$maiacRaster, max(rasterList$ws_raster[[1:23]]), left_title="MAIAC Swath\n2016-07-21 AQUA Passes (2)", right_title="6km CANSAC Model\n2016-07-21 Maximum")
#' # for ( i in 1:23 ) {rasterMap_sideBySide(rasterList$maiacRaster, rasterList$ws_raster[[i]], left_title="MAIAC Swath\n2016-07-21 AQUA Passes (2)")}
#' }

rasterMap_sideBySide <- function(left_raster, right_raster,
                                 left_title=NULL, right_title=NULL,
                                 left_colors=NULL, right_colors=bs_colors$colors,
                                 left_breaks=NULL, right_breaks=NULL) {

  # ----- Setup -----------------
  
  # TODO: save old par
  oldpar <- par(mfrow=c(1,2))
  
  # TODO: add functionality to crop to one or the other.....currently can plot rasters from two totally different areas
  # TODO: make it optional, but default to crop to minimum shared area between the two...
  
  # ----- Plot settings -----------------
  
  # settings
  # TODO: add logic here for default plot settings that apply to both plots

  # title
  if (is.null(left_title)) {
    left_title <- names(left_raster)
  }
  if (is.null(right_title)) {
    right_title <- names(right_raster)
  }
  
  # colors
  if (is.null(left_colors)) {
    left_colors <- aot_colors$colors
  }
  if (is.null(right_colors)) {
    right_colors <- bs_colors$colors
  }
  
  # breaks
  if (is.null(left_breaks)) {
    left_breaks <- aot_colors$breaks
  }
  if (is.null(right_breaks)) {
    right_breaks <- bs_colors$breaks
  }
  
  
  # ----- Create Plots -------------
  
  plot(left_raster, main=left_title, col=left_colors, breaks=left_breaks)
  map(database="state", add=TRUE)
  
  plot(right_raster, main=right_title, col=right_colors, breaks=right_breaks)
  map(database="state", add=TRUE)

  # ----- Cleanup -------------------
  
  par(oldpar)
  
}
