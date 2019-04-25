#' @export
#' 
#' @title Create a RasterBrick of GOES data
#' 
#' @param nc ncdf4 handle
#' @param res resolution of raster in degrees
#' @param fun function to use when rasterizing
#' @param xmn lower longitude extent
#' @param xmx upper longitude extent
#' @param ymn lower latitude extent
#' @param ymx upper latitude extent
#' @param dqfLevel data quality flag level
#' 
#' @description Create a raster of GOES AOD data including data points
#' with the specified resolution, within the specified extent and data quality 
#' flag level. Data quality level can take a value of:
#' 
#' 0: High quality retrieval flag
#' 1: Medium quality retrieval flag
#' 2: Low quality retrieval flag
#' 3: No retrieval quality flag
#' 
#' @return RasterBrick

goesaodc_createRaster <- function(
  nc,
  res = 0.1,
  fun = mean,
  xmn = NULL,
  xmx = NULL,
  ymn = NULL,
  ymx = NULL,
  dqfLevel = NULL
) {
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  pts <- goesaodc_createSpatialPoints(nc = nc,
                                  xmn = xmn, xmx = xmx,
                                  ymn = ymn, ymx = ymx,
                                  dqfLevel = dqfLevel)
  
  # ----- Create Raster --------------------------------------------------------
  
  extent <- raster::extent(pts)
  
  lon_min <- extent@xmin; lon_max <- extent@xmax
  lat_min <- extent@ymin; lat_max <- extent@ymax
  ncols <- ((lon_max - lon_min)/res)+1
  nrows <- ((lat_max - lat_min)/res)+1
  
  raster <- raster::raster(nrows=nrows, ncols=ncols,
                           xmn=lon_min, xmx=lon_max,
                           ymn=lat_min, ymx=lat_max,
                           res=res,
                           crs="+proj=longlat +datum=WGS84 +ellps=GRS80")
  
  raster <- raster::rasterize(pts, raster, fun=fun)
  
  return(raster)
}

# ===== Debugging ==============================================================

if (FALSE) {
  filePath <- "/Users/tom/Projects/MazamaSatelliteUtils/local_data/OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc"
  nc <- nc_open(filePath)
  raster <- goesaodc_createRaster(nc)
  
  # plot data
  loadSpatialData('USCensusStates')
  plot(raster$AOD)
  map("state", add=T)
  plot(SimpleCountries, add=TRUE)

  # plot just Pennsylvania
  pa <- subset(USCensusStates, stateCode == "PA")
  bb_pa <- bbox(pa)
  raster <- goesaodc_createRaster(nc, 
                              xmn = bb_pa['x','min'], 
                              xmx = bb_pa['x','max'], 
                              ymn = bb_pa['y','min'], 
                              ymx = bb_pa['y','max'])
  plot(raster$AOD)
  map("state", add=T)
}
