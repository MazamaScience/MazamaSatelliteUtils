#' @export
#' 
#' @title Open a GOES AOD netCDF file from the satelliteDataDir
#' 
#' @param fileName file name of GOES AOD file in the satelliteDataDir
#' 
#' @return nc handle or NULL if no file was found

goesaodc_openFile <- function(
  fileName
) {
  fullPath <- paste0(getSatelliteDataDir(), "/", fileName)
  if (file.exists(fullPath)) {
    nc <- ncdf4::nc_open(fullPath)
    return(nc)
  } else {
    print("File not found")
    return(NULL)
  }
}
