#' @export
#' @title Create a tibble from ncdf4 handle
#' @param nc ncdf4 handle
#' @param vars list of variables
#' @description Create a tibble with columns: AOD, DQF, lon, and lat
#' @return tibble

goes_createTibble <- function(
  nc,
  vars = c("AOD", "DQF")
) {
  
  # check that nc has GOES projection
  if (!isGoesProjection(nc)) {
    stop(paste0("ncdf4 object does not have GOES projection"))
  }
  
  # get latitude and longitude from goesEastGrid and flatten
  lat <- as.numeric(goesEastGrid$latitude)
  lon <- as.numeric(goesEastGrid$longitude)
  
  # remove invalid latitudes and longitudes
  mask <- !is.na(lon) & !is.na(lat)
  valid_lon <- lon[mask]
  valid_lat <- lat[mask]
  
  # extract vars from nc
  varList <- list(lon=valid_lon, lat=valid_lat)
  for (var in vars) {
    variable <- ncdf4::ncvar_get(nc, var)
    variable <- as.numeric(variable)[mask] # apply mask to remove data with invalid latitudes and longitudes
    variable <- list(variable)
    names(variable) <- var
    varList <- c(varList, variable)
  }
  
  tbl <- tibble::as_tibble(varList)
  
  return(tbl)
}


if (FALSE) {
  filePath <- "/Users/tom/Projects/MazamaSatelliteUtils/local_data/OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc"
  nc <- nc_open(filePath)
  tbl <- goes_createTibble(nc)
}