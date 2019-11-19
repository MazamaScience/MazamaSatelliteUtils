#' @export
#' @title Download or load MAIAC data and return data as a dataframe
#' @param date desired date (integer, character representing YYYYMMDD[HH] or datetime object)
#' @param julianDate desired date on Julian calendar (YYYYDDD). Ignored if 'date' is specified.
#' @param time UTC hour and minute for data (HHMM). Optional when only one file for the specified date.
#' @param product product code (MAIACAAOT | MAIACABRF | MAIACRTLS | MAIACTAOT | MAIACTBRF)
#' @param tileNumber number code for tile (eg h01v04). 
#' @param baseUrl base URL for data queries
#' @return Raw tibble of MAIAC data
#' @description load a raw dataframe
#' @examples 
#' \dontrun{
#' maiac <- maiac_loadRawDataframe("h01v04", 20171009, 2150)
#' }

maiac_loadRawDataframe <- function(
  tileNumber = NULL,
  date = NULL, 
  time = NULL,
  julianDate = NULL,
  product = "MAIACAAOT",
  baseUrl = NULL
) {
  
  # Glue the BaseURL back together 
  portal_url <- "https://portal.nccs.nasa.gov/"
  maiac_url <- "datashare/maiac/DataRelease/NorthAmerica_2000-2016/"
  baseURL <- paste0(portal_url, maiac_url)
  
  hdffilePath <- maiac_downloadNorthAmerica(tileNumber, date, time, 
                                            julianDate, product, baseUrl)
  nc4filePath <- maiac_2nc4(hdffilePath)
  
  nc <- ncdf4::nc_open(nc4filePath)
  
  # ----- Get variables -------------------------------------------------------
  
  lat <- as.numeric(ncdf4::ncvar_get(nc, "grid1km_latitude"))
  lon <- as.numeric(ncdf4::ncvar_get(nc, "grid1km_longitude"))
  aot_047 <- as.numeric(ncdf4::ncvar_get(nc, "Optical_Depth_047"))
  aot_055 <- as.numeric(ncdf4::ncvar_get(nc, "Optical_Depth_055"))
  aot_model <- as.character(ncdf4::ncvar_get(nc, "AOT_MODEL"))
  
  toBitString <- function(x) { 
    return( paste0( tail(rev(as.numeric(intToBits(x))),16) , collapse="") ) 
  }
  aot_qa <- as.numeric(ncdf4::ncvar_get(nc, "AOT_QA"))
  for (i in 1:length(unique(aot_qa))) {
    qa <- unique(aot_qa)[i]
    bin <- toBitString(qa)
    aot_qa[which(aot_qa == qa)] <- bin
  }
  # See MAIACData_UsrGd.pdf for QA codes
  
  aot_uncertainty <- as.numeric(ncdf4::ncvar_get(nc, "AOT_Uncertainty"))
  # AOD uncertainty: This parameter is evaluated based on the Blue-band B3 
  # surface brightness (reflectance) only, and thus gives only a general 
  # indication of possible increase of error over brighter surfaces;
  
  tbl <- tibble::data_frame(latitude = lat,
                            longitude = lon,
                            aot_055,
                            aot_047,
                            aot_model,
                            aot_qa,
                            aot_uncertainty)
  
  # Return
  return(tbl)
  
}
