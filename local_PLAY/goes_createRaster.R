# goes_createRaster() prototype

# getting `Error: vector memory exhausted (limit reached?)` error from the last
# line of this function. I tried restarting RStudio and running the example below
# with nothing but filePath and goes_createRaster in the environment, the problem
# still persists.

if (FALSE) {
  filePath <- "/Users/tom/Projects/MazamaSatelliteUtils/local_data/OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc"
  raster <- goes_createRaster(filePath, xmn = 500, xmx = 800, ymn = 500, ymx = 800)
}

goes_createRaster <- function(
  filePath,
  var = "AOD",
  reso = 0.1,
  fun = mean,
  xmn = NULL,
  xmx = NULL,
  ymn = NULL,
  ymx = NULL
) {
  
  # sanity check
  if (!file.exists(filePath)) {
    stop(paste0(filePath, " not found"))
  }
  
  # load .nc
  nc <- ncdf4::nc_open(filePath)
  
  # check that projection matches GOES projection
  if (!isGoesProjection(nc)) {
    stop(paste0(filePath, " not GOES projection"))
  }
  
  # extract AOD
  variable <- ncdf4::ncvar_get(nc, var)
  
  # set min/max variables for subsetting
  if (is.null(xmn)) {xmn = 0} 
  if (is.null(xmx)) {xmx = dim(goesEastGrid$longitude)[1]} 
  if (is.null(ymn)) {ymn = 0} 
  if (is.null(ymx)) {ymx = dim(goesEastGrid$longitude)[2]}
  
  # flatten lon, lat, aod, and subset
  lon <- as.numeric(goesEastGrid$longitude[xmn:xmx, ymn:ymx])
  lat <- as.numeric(goesEastGrid$latitude[xmn:xmx, ymn:ymx])
  variable <- as.numeric(variable[xmn:xmx, ymn:ymx])

  # drop all data with invalid latitudes and longitudes
  mask <- !is.na(lon) & !is.na(lat)
  valid_lon <- lon[mask]
  valid_lat <- lat[mask]
  valid_variable <- variable[mask]

  # convert to SpatialPointsDataFrame
  variable_coords <- cbind(valid_lon, valid_lat)
  variable_pts <- sp::SpatialPointsDataFrame(coords=variable_coords, 
                                         data=data.frame(valid_variable))

  cell_size = reso  # resolution in degrees
  lon_min <- min(valid_lon); lon_max <- max(valid_lon)
  lat_min <- min(valid_lat); lat_max <- max(valid_lat)
  ncols <- ((lon_max - lon_min)/cell_size)+1
  nrows <- ((lat_max - lat_min)/cell_size)+1

  raster <- raster::raster(nrows=nrows, ncols=ncols,
                           xmn=lon_min, xmx=lon_max,
                           ymn=lat_min, ymx=lat_max,
                           res=cell_size,
                           crs="+proj=longlat +datum=WGS84 +ellps=GRS80")

  raster <- raster::rasterize(variable_pts, raster, fun=fun)
  names(raster)[2] <- var

  return(raster)
}

