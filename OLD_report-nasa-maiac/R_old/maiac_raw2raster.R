#' export
#' @title Convert hdf file to .nc file
#' @param maiac_raw raw maiac dataframe
#' @param raster template raster
#' @param param parameter to use for raster values
#' @description rasterizes raw maiac data 
#' @return raster::Raster* object

maiac_raw2raster <- function(maiac_raw, 
                             raster=NULL, 
                             param="aot_055") {
  
  if (is.null(raster)) {
    raster(nrows = 1200, 
           ncols = 1200, 
           xmn = range(maiac_raw$longitude)[1], 
           xmx = range(maiac_raw$longitude)[2],
           ymn = range(maiac_raw$latitude)[1], 
           ymx = range(maiac_raw$latitude)[2], 
           crs = sp::CRS("+init=epsg:4326"))
  }
  
}
