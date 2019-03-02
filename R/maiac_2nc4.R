#' @export
#' @title Convert hdf file to .nc file
#' @param file absolute path of file to be converted
#' @param converterPath location of converter function
#' @param cleanup logical specifying whether to remove original .hdf file
#' @description Converts .hdf file to .nc file using h4toncff_nc4 http://www.hdfeos.org/software/h4cflib.php
#' @return Absolute path of converted NetCDF file.
#' @examples 
#' \dontrun{
#' hdfPath <- maiac_downloadNorthAmerica("h01v04", 20171009, 2150)
#' ncdfPath <- maiac_2nc4(hdfPath)
#' nc <- ncdf4::nc_open(ncdfPath)
#' nc
#' }

maiac_2nc4 <- function (
  file, 
  converterPath = "./executables/h4toncff_nc4", 
  cleanup = TRUE
) {
  
  if ( !file.exists(converterPath) ) {
    stop(paste0("invalid converterPath: ", converterPath))
  }
  
  if ( !file.exists(file) ) {
    stop(paste0(file, " does not exist"))
  }
  
  newFilePath <- stringr::str_replace(file, "\\.hdf$", ".nc")
  
  if ( file.exists(newFilePath) ) {
    # If file already exists, do nothing
    message(paste0(newFilePath, " already exists. Skipping conversion"))
  } else {
    command <- paste0(converterPath, " ", file, " ", newFilePath)
    output <- system(command, intern = TRUE)
  }
  
  return(invisible(newFilePath))
  
}
