#' @export
#' @title Convert MAIAC Swath Lat/Lon/AOT Data to Raster
#' @param maiac MAIAC swath to convert
#' @param ws_raster raster upon which to apply the MAIAC swath data
#' @param fun function to apply when multiple points per grid cell
#' @param crop crop the output raster to the lat/lon range of the maiac swath
#' @description Fill a RasterGrid with data from a MAIAC swath
#' @examples 
#' \dontrun{
#' load("~/Projects/airfire-nasa-maiac/localData/Sand_bluesky_CANSAC_6km.RData")
#' maiac <- loadRawMaiac("~/Projects/airfire-nasa-maiac/localData/maiac_sands_AQUA_20160721.nc")
#' ws_raster <- convertGridToRaster(Sand_bluesky_CANSAC_6km)
#' maiacRaster <- convertSwathToRaster(maiac, ws_raster)
#' plot(maiacRaster)
#' }

# # testing
# 
# if ( FALSE ) {
#   
#   library(raster)
#   library(PWFSLSmokeModeling)
#   source('~/Projects/airfire-nasa-maiac/R/convertGridToRaster.R')
#   source('~/Projects/airfire-nasa-maiac/R/convertRasterToGrid.R')
#   source('~/Projects/airfire-nasa-maiac/R/loadRawMaiac.R')
#   source('~/Projects/airfire-nasa-maiac/R/convertSwathToRaster.R')
#   load("~/Projects/airfire-nasa-maiac/localData/Sand_bluesky_CANSAC_2km.RData")
#   ws_grid <- Sand_bluesky_CANSAC_2km; rm(Sand_bluesky_CANSAC_2km)
#   maiac <- loadRawMaiac("~/Projects/airfire-nasa-maiac/localData/maiac_sands_AQUA_20160721.nc")
# 
#   # convert ws_grid to raster
#   ws_raster <- convertGridToRaster(ws_grid)
#   
#   # crop raster to match swath bounds
#   ext <- extent(c(range(maiac$longitude), range(maiac$latitude)))
#   ws_raster <- crop(ws_raster, ext)
#   
#   # let's see how many points per cell...
#   maiacRaster_counts <- convertSwathToRaster(maiac, ws_raster, fun='count')
#   plot(maiacRaster_counts, main="number of counts per cell")
# 
#   # For the number of unique values per grid cell:
#   maiacRaster_unique <- convertSwathToRaster(maiac, ws_raster, fun=function(x, ...){ length(unique(na.omit(x)))})
#   plot(maiacRaster_unique, main="number of unique values per cell")
#   
#   # min
#   maiacRaster_min <- convertSwathToRaster(maiac, ws_raster, fun='min')
#   plot(maiacRaster_min, main="min value per cell")
#   
#   # last
#   maiacRaster <- convertSwathToRaster(maiac, ws_raster)
#   plot(maiacRaster, main="last value per cell")
#   
#   # max
#   maiacRaster_max <- convertSwathToRaster(maiac, ws_raster, fun='max')
#   plot(maiacRaster_max, main="max value per cell")
#   
#   # subset and plot actual grid data (let's do it on top of "last" plot)
#   plot(maiacRaster, main="last value per cell")
#   breaks <- c(-10000,1,5,10,20,40,90,140,350,525,10000)
#   colors <- c("transparent", heat.colors(length(breaks)))
#   plot(ws_raster[[34]], col=colors, breaks=breaks, add=TRUE, legend=FALSE )
#   
# }

# TODO: update default function to mean or median

convertSwathToRaster <- function(maiac, ws_raster, fun="last", crop=TRUE) {

  # crop grid to match swath bounds
  if ( crop ) {
    ext <- raster::extent(c(range(maiac$longitude), range(maiac$latitude)))
    ws_raster <- raster::crop(ws_raster, ext)
  }
  
  maiacRaster <- raster::rasterize(x=maiac[c("longitude","latitude")], y=ws_raster, field=maiac$aot, fun=fun)
  
  # return raster object of maiac AOT swatch converted to same grid as ws_grid
  return(maiacRaster)
  
}
