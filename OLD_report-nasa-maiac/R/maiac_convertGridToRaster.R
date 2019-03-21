#' @export
#' @title Convert a PWFSLSmokeModeling ws_grid Object into a RasterBrick Object
#' @param ws_grid ws_grid object
#' @param time time (YYYYMMDDHH) to use. If null, all times are included as separate layers.
#' @description Converts a ws_grid object into RasterBrick object, where each layer corresponds to
#' one of the data layers in the original ws_grid object. The layers usually correspond to timestamps 
#' alone but may also refer to additional pollutants and/or elevations.
#' 
#' Layer names are given per the following convention: parameter_elevation_timestamp, e.g. pm25_100_2017072201
#' 
#' @return RasterBrick object
#' @examples
#' \dontrun{
#' PWFSLSmokeModeling::setModelDataDir('~/Data/Bluesky')
#' ws_grid <- PWFSLSmokeModeling::bluesky_load(model = "CANSAC-6km", modelRun = 20160722)
#' ws_raster <- convertGridToRaster(ws_grid)
#' PWFSLSmokeModeling::gridMap(ws_grid, slice=20)
#' plot(ws_raster[[20]])
#' }

convertGridToRaster <- function(ws_grid, time = NULL) {

  # basic checks
  if ( !("ws_grid" %in% class(ws_grid)) ) {
    stop("ws_grid must be of class ws_grid")
  }
  
  # get data dimensions
  len_x <- length(ws_grid$longitude)
  len_y <- length(ws_grid$latitude)
  len_z <- length(ws_grid$elevation)
  len_t <- length(ws_grid$time)
  
  if (is.null(time)) {
    timeSlices <- 1:len_t
  } else {
    timeSlices <- which(ws_grid$time == PWFSLSmoke::parseDatetime(time))
    if(length(timeSlices) == 0) {
      warning(paste0(PWFSLSmoke::parseDatetime(time), " not included in dataset. Returning empty raster."))
    }
  }
  
  # set lat/lon range; offset by half a grid cell in each direction since lat/lon pairs mark pixel centerpoints
  xmn <- range(ws_grid$longitude)[1]-ws_grid$deltaLon/2
  xmx <- range(ws_grid$longitude)[2]+ws_grid$deltaLon/2
  ymn <- range(ws_grid$latitude)[1]-ws_grid$deltaLat/2
  ymx <- range(ws_grid$latitude)[2]+ws_grid$deltaLat/2

  rasterList <- list()
  
  for ( parameter in names(ws_grid$data) ) {
    for ( elevSlice in 1:len_z ) {
      
        for ( timeSlice in timeSlices ) {
          
          # assign layerName based on current position in nested loops
          elevation <- ws_grid$elevation[elevSlice]
          timestamp <- as.character(ws_grid$time[timeSlice], "%Y%m%d%H")
          layerName <- paste0(parameter, "_", elevation, "_", timestamp)
          
          # pull out data slice
          if ( len_z == 1 ) {
            data <- ws_grid$data[[parameter]][, , timeSlice]
          } else {
            data <- ws_grid$data[[parameter]][, , elevSlice, timeSlice]
          }
          
          # reorient the data
          data <- t(data[, ncol(data):1])
          
          # build raster for current layer
          rasterList[[layerName]] <- raster(nrow=len_y, ncol=len_x, xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx, vals=data,
                                            crs = sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"))
          
        }  
    }
  }
  
  # combine layers into a single RasterBrick object
  ws_raster <- raster::brick(rasterList)
  
  return(ws_raster)

}
