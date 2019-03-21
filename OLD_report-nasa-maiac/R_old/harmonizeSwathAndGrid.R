#' @export
#' @title Convert MAIAC and ws_grid Object to Rasters With the Same Resolution and Extent
#' @param maiac maiac swath data
#' @param ws_grid ws_grid object
#' @param gridSlice index of ws_grid to return as raster
#' @param fun function to apply when multiple points per grid cell
#' @param cropToMaiac crop the maps to the lat/lon range of the maiac swath
#' @description Convert a maiac swath and a ws_grid object to rasters with the same resolution and extent;
#' useful for plotting from raw data
#' @examples 
#' \dontrun{
#' load('~/Projects/airfire-nasa-maiac/localData/Sand_bluesky_CANSAC_6km.RData')
#' maiac <- loadRawMaiac('~/Projects/airfire-nasa-maiac/localData/maiac_sands_AQUA_20160721.nc')
#' rasterList <- harmonizeSwathAndGrid(maiac, Sand_bluesky_CANSAC_6km, 58, cropToMaiac=FALSE)
#' }

harmonizeSwathAndGrid <- function(maiac, ws_grid, gridSlice=NULL, fun='last', cropToMaiac=TRUE) {
  
  # ----- Data Setup --------------
  
  # convert ws_grid to ws_raster
  ws_raster <- convertGridToRaster(ws_grid)

  # subset ws_raster to a single slice (useful for plotting)
  if ( !is.null(gridSlice) ) {
    ws_raster <- ws_raster[[gridSlice]]
  }
  
  # convert maiac swath to raster
  maiacRaster <- convertSwathToRaster(maiac, ws_raster, fun, crop=FALSE)
  
  # crop settings
  if ( cropToMaiac ) {
    extent <- raster::extent(c(range(maiac$longitude), range(maiac$latitude)))
    ws_raster <- raster::crop(ws_raster, extent)
    maiacRaster <- raster::crop(maiacRaster, extent)
  }
  
  return(list(maiacRaster=maiacRaster, ws_raster=ws_raster))
  
}
