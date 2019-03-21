#' @export
#' @title Export MAIAC file to csv for polygon plotting
#' @param file path to MAIAC or other .nc file
#' @param newFilename full path for new filename; defaults to given filename with .csv extension
#' @description Save a .nc file as a .csv for plotting with spatial polygons or other related tasks
#' @examples
#' \dontrun{
#' exportMaiacAsCsv("./localData/maiac_sands_AQUA_20160721.nc")
#' }

exportMaiacAsCsv <- function(file, newFilename=NULL) {
  
  maiac <- loadRawMaiac(file)
  maiac <- data.frame(latitude=maiac$latitude, longitude=maiac$longitude, aot=maiac$aot)
  
  if ( is.null(newFilename) ) {
    newFilename <- stringr::str_replace(file, ".nc", ".csv")
  }
  
  readr::write_csv(x=maiac, path=newFilename)
  
}
