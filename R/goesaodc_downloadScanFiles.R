#' @export
#'
#' @title Download GOES AOD scan files
#'
#' @description Downloads GOES AOD scan files for a given datetime or 
#' time range. If just \code{datetime} is given, then only the scan file closest
#' to that time will be downloaded. If \code{endtime} is specified as well, then
#' all scans from \code{datetime} up to (but not including) \code{endtime} will 
#' be downloaded.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' goesaodc_downloadScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:30",
#'   timezone = "UTC"
#' )
#' 
#' goesaodc_downloadScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "UTC",
#'   verbose = TRUE
#' )
#' }

goesaodc_downloadScanFiles <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  isJulian = FALSE,
  filenames = NULL,
  verbose = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # ----- Determine which scan files to download -------------------------------
  
  if ( is.null(filenames) ) {
    filenames <- goesaodc_listScanFiles(
      satID = satID,
      datetime = datetime,
      endtime = endtime,
      timezone = timezone,
      isJulian = isJulian,
      useRemote = TRUE,
      baseUrl = baseUrl
    )
  }
  
  # TODO: Identify scan files not already on disk
  
  # ----- Download scan files --------------------------------------------------
  
  for ( i in 1:length(filenames) ) {
    
    filename <- filenames[i]
    
    if ( is.null(satID) ) {
      MazamaCoreUtils::stopIfNull(filename)
      filePattern <- "OR_ABI-L2-AODC-M[0-9]_(G16|G17)_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
      satID <- stringr::str_match(filename, filePattern)[1,2]
    }
    
    fileUrl <- paste0(
      "https://tools-1.airfire.org/Satellite/",
      ifelse(satID == "G16", "GOES-16/AODC/", "GOES-17/AODC/"),
      filename
    )
    
    filePath <- file.path(getSatelliteDataDir(), filename)
    
    result <- try({
      utils::download.file(
        fileUrl,
        destfile = filePath, 
        quiet = TRUE, 
        method = "auto", 
        mode = "wb"
      )
    }, silent = FALSE)
    
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      if ( MazamaCoreUtils::logger.isInitialized() ) {
        MazamaCoreUtils::logger.warn(err_msg)
      }
    } else {
      if ( verbose ) {
        message(paste0("Downloaded (", i, "/", length(filenames), "): ", filename))
      }
    }
    
  }
  
  return(filenames)
  
}
