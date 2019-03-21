#' @export
#' @title convert RasterBrick to PWFSLSmokeModeling ws_grid object
#' @param ws_raster RasterBrick Object to be converted, with proper layer names (see Details for more)
#' @description Converts model-based RasterBrick into ws_grid object
#' @return ws_grid object
#' @details Layer names must adhere to the following naming convention: parameter_elevation_timestamp (e.g. pm25_100_2016072105)
#' @examples
#' \dontrun{
#' PWFSLSmokeModeling::setModelDataDir('~/Data/Bluesky')
#' ws_grid <- PWFSLSmokeModeling::bluesky_load(model = "CANSAC-6km", modelRun = 20160722)
#' ws_raster <- convertGridToRaster(ws_grid)
#' ws_grid2 <- convertRasterToGrid(ws_raster)
#' PWFSLSmokeModeling::gridMap(ws_grid, slice=6)
#' PWFSLSmokeModeling::gridMap(ws_grid2, slice=6)
#' }

convertRasterToGrid <- function(ws_raster) {

  # ----- Initial setup ----------------------------------
  
  ws_grid <- list()
  
  # pull out layer info (assumes layer names follow this format: parameter_elevation_timestamp)
  layerNames <- names(ws_raster)
  layerInfo <- data.frame(do.call(rbind, strsplit(layerNames, "_", fixed=TRUE)))
  colnames(layerInfo) <- c("parameters","elevations","timestamps")
  parameters <- as.character(levels(unique(layerInfo$parameters)))
  elevations <- as.numeric(levels(unique(layerInfo$elevations)))
  timestamps <- as.numeric(levels(unique(layerInfo$timestamps)))
  
  # pull out data dimensions
  len_x <- dim(ws_raster)[1]
  len_y <- dim(ws_raster)[2]
  # len_z <- length(elevations)
  # len_t <- length(timestamps)

  # ----- Create deltaLon and deltaLat -------------------
  
  ws_grid$deltaLon <- (ws_raster@extent@ymax-ws_raster@extent@ymin)/len_x
  ws_grid$deltaLat <- (ws_raster@extent@xmax-ws_raster@extent@xmin)/len_y
  
  # ----- Create longitude and latitude ------------------
  
  ws_grid$longitude <- seq(ws_raster@extent@xmin+ws_grid$deltaLon/2, by=ws_grid$deltaLon, length.out=len_y)
  ws_grid$latitude <- seq(ws_raster@extent@ymin+ws_grid$deltaLat/2, by=ws_grid$deltaLat, length.out=len_x)  
  
  # ----- Create time ------------------------------------
  
  ws_grid$time <- PWFSLSmoke::parseDatetime(timestamps)
  
  # ----- Create elevation -------------------------------
  
  ws_grid$elevation <- elevations
  
  # ----- Create model -----------------------------------
  
  ws_grid$model <- "ws_raster"
  
  # ----- Create modelRun --------------------------------
  
  ws_grid$modelRun <- "ws_raster"
  
  # ----- Create data ------------------------------------
  
  # NOTE: need to create X/Y/Z block for every individual time stamp
  # NOTE: So, for each time slice, stack the elevations
  # NOTE: Then, after each time's X/Y/Z block has been created, bind these in the 4th dimension (time)
  # NOTE: Drop the elevation dimension if only one
  
  dataList_XYZT_byParameter <- list()
  
  for ( parameter in parameters ) {

    # For each timestamp, bind data for all elevations
    dataList_XYZ_byTimestamp <- list()
    
    for ( timestamp in timestamps ) {
      
      # Identify raster layer indices corresponding to current timestamp
      layerIDs <- which(layerInfo$timestamps == timestamp)
      
      # For current timestamp, create a data list where each element holds XY data for a single elevation
      dataList_XY_byElevation <- list()
      for ( layerID in layerIDs ) {
        layerData <- matrix(ws_raster[[layerID]]@data@values, len_y, len_x)
        layerData <- layerData[, ncol(layerData):1]
        dataList_XY_byElevation[[as.character(layerInfo$elevations[layerID])]] <- layerData
      }
      
      # stack data at each elevation into single 3D data block
      dataList_XYZ_byTimestamp[[as.character(timestamp)]] <- abind::abind(dataList_XY_byElevation, along = 3)
    
    }
    
    # Merge the XYZ data for each timestamp into a single XYZT block for the current parameter
    data_XYZT <- abind::abind(dataList_XYZ_byTimestamp, along = 4)
    
    # drop elevation dimension if only one
    if ( length(elevations) == 1 ) {
      data_XYZT <- drop(data_XYZT)
    }
    
    dataList_XYZT_byParameter[[parameter]] <- data_XYZT
    
  }
  
  ws_grid$data <- dataList_XYZT_byParameter

  # ----- Cleanup ------------------------------------
  
  # reorder elements and define as ws_grid object
  ws_grid <- ws_grid[c("longitude", "latitude", "elevation", "time", "data", "model", "modelRun", "deltaLon", "deltaLat")]
  ws_grid <- structure(ws_grid, class=c("ws_grid","list"))
  
}
