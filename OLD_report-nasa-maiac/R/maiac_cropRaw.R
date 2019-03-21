#' @export
#' @title Crop raw maiac tibble
#' @param maiac_rawDataframe maiac dataframe generated from maiac_loadRawDataframe()
#' @param xlim low and high longitude values (xlo, xhi)
#' @param ylim low and high latitude values (ylo, yhi)
#' @return cropped raw maiac tibble
#' @description crop a raw maiac dataframe
#' @examples 
#' \dontrun{
#' maiac <- maiac_loadRawDataframe("h01v04", 20171009, 2150)
#' maiac <- maiac_cropRaw(maiac, xlim = c(-125.52, -117.03), ylim = c(36.52, 41.78))
#' }


maiac_cropRaw <- function(maiac_rawDataframe, xlim = NULL, ylim = NULL) {
  
  lonMask <- which(maiac_rawDataframe$longitude > xlim[1] & maiac_rawDataframe$longitude < xlim[2])
  maiac <- maiac_rawDataframe[lonMask,]
  
  latMask <- which(maiac$latitude > ylim[1] & maiac$latitude < ylim[2])
  maiac <- maiac[latMask,]
  
  return(maiac)
}
