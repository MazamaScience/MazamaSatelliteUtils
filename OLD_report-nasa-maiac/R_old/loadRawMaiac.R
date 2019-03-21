#' @export
#' @title Read in a Raw MAIAC NetCDF file
#' @param file absolute path of file to be converted
#' @description Converts NASA MAIAC swath data from its origin format into a dataframe
#' containing longitude, latitude, and aot.
#' @return Dataframe with three columns.
#' @examples 
#' \dontrun{
#' maiac <- loadRawMaiac("./localData/maiac_sands_AQUA_20160721.nc")
#' }

loadRawMaiac <- function(file) {
  
  # open nc file
  nc <- ncdf4::nc_open(file)
  
  # ----- Get dimensions ------------------------------------------------------
  
  ncl0 <- nc$dim$ncl0$vals
  ncl1 <- nc$dim$ncl1$vals
  ncl2 <- nc$dim$ncl2$vals
  
  if ( ! all.equal(ncl0, ncl1, ncl2) ) stop("Dimensions differ.")
  
  # ----- Get variables -------------------------------------------------------

  lat <- ncdf4::ncvar_get(nc, "lat")
  lon <- ncdf4::ncvar_get(nc, "lon")
  aot_055 <- ncdf4::ncvar_get(nc, "aot_055")
  
  df <- data.frame(longitude=as.numeric(lon),
                   latitude=as.numeric(lat),
                   aot=as.numeric(aot_055))
  
  # Return
  return(df)
  
}
