#' @export
#' @title Download North America MAIAC swath data
#' @param date desired date (integer, character representing YYYYMMDD[HH] or datetime object)
#' @param julianDate desired date on Julian calendar (YYYYDDD). Ignored if 'date' is specified.
#' @param time UTC hour and minute for data (HHMM). Optional when only one file for the specified date.
#' @param product product code (MAIACAAOT | MAIACABRF | MAIACRTLS | MAIACTAOT | MAIACTBRF)
#' @param tileNumber number code for tile (eg h01v04). 
#' @param baseUrl base URL for data queries
#' @param filePath optional path of .nc file. If not null, all other arguments are ignored.
#' @param params vector of variable names to use for the values of the grid cells. Grid size must match. 
#' "all_1km" or "all_5km" may also be used to include layers for all parameters on the 1km grid or 5km grid. 
#' @param converterPath Path to executable for converting HDF4 to netCDF4
#' @description Load North America MAIAC swath data as a raster
#' @return raster::Raster* object
#' @examples 
#' \dontrun{
#' napaSmoke <- maiac_loadRaster( "h01v04", 20171009, 2150 )
#' plot(napaSmoke)
#' }

maiac_loadRaster <- function(
  tileNumber = NULL,
  date = NULL, 
  time = NULL,
  julianDate = NULL,
  product = "MAIACAAOT",
  baseUrl = "https://portal.nccs.nasa.gov/datashare/maiac/DataRelease/NorthAmerica_2000-2016/",
  filePath = NULL,
  params = "Optical_Depth_055",
  converterPath = "./executables/h4toncff_nc4"
) {
  
  # Load data
  
  if ( !is.null(filePath) ) {
    if ( !file.exists(filePath) ) {
      stop(paste0(filePath, " not found"))
    } else {
      nc4filePath <- filePath
      
    }
  } else {
    hdffilePath <- maiac_downloadNorthAmerica(tileNumber, date, time, 
                                              julianDate, product, baseUrl)
    nc4filePath <- maiac_2nc4(hdffilePath, converterPath = converterPath)
    
  }
  
  nc <- ncdf4::nc_open(nc4filePath)
  
  
  # ----- Get variables --------------------------------------------------------
  meta <- maiac_getMetadata(nc4filePath)
  if ( length(params) == 1 ) {
    if ( params == "all_1km" ) {
      params <- meta$grid_1km$vars
    } else if (params == "all_5km") {
      params <- meta$grid_5km$vars
    }
  }
  
  
  if ( any(params %in% meta$grid_1km$vars) && 
       any(params %in% meta$grid_5km$vars) ) {
    stop("cannot load params from both 1km and 5km grids")
  } else {
    grid <- ifelse(params[1] %in% meta$grid_1km$vars, "grid_1km", "grid_5km") 
  }
  meta <- meta[[grid]]
  
  templateRaster <- raster::raster(
    nrows = meta$ydim, ncol = meta$xdim,
    xmn = meta$xmn, xmx = meta$xmx, 
    ymn = meta$ymn, ymx = meta$ymx,
    crs = sp::CRS("+proj=aea +lat_1=20 +lat_2=60 +lon_0=-96 +lat_0=23 +datum=WGS84 +ellps=WGS84")
  )
  
  if ( length(params) == 1 ) {
    vals <- ncdf4::ncvar_get(nc, params) 
    maiacRaster <- setValues(templateRaster, t(vals))
  } else {
    maiacRaster <- templateRaster
    for ( param in params ) {
      vals <- ncdf4::ncvar_get(nc, param)
      layer <- maiacRaster
      layer <- setValues(templateRaster, t(vals))
      maiacRaster <- addLayer(maiacRaster, layer)
    }
  }
  names(maiacRaster) <- params
  
  return(maiacRaster)
  
}