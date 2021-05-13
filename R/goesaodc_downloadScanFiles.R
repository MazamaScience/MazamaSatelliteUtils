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
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime as a Ymd HMS or Julian formatted string, or a 
#' \code{POSIXct}.
#' @param endtime End time as a Ymd HMS or Julian formatted string, or a 
#' \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}. Defaults to UTC.
#' @param isJulian Logical flag determining whether \code{datetime} (and
#' optionally \code{endtime}) should be interpreted as a Julian date with day of
#' year as a decimal number. Defaults to FALSE.
#' @param filenames Names of scan files.
#' @param verbose Logical flag determining whether to print download progress 
#' messages.
#' @param baseUrl URL of remote database. Defaults to 
#' "https://tools-1.airfire.org/Satellite/".
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
  
  requestedFilenames <- if ( is.null(filenames) ) {
    goesaodc_listScanFiles(
      satID = satID,
      datetime = datetime,
      endtime = endtime,
      timezone = timezone,
      isJulian = isJulian,
      useRemote = TRUE,
      baseUrl = baseUrl
    )
  } else {
    filenames
  }
  
  # Only download the requested files that aren't already available locally
  requestedFilepaths <- file.path(getSatelliteDataDir(), requestedFilenames)
  missingFilenames <- requestedFilenames[!file.exists(requestedFilepaths)]
  
  # ----- Download scan files --------------------------------------------------
  
  i <- 1
  for ( filename in missingFilenames ) {
    
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
      if ( MazamaCoreUtils::logger.isInitialized() )
        MazamaCoreUtils::logger.warn(err_msg)
    } else {
      if ( verbose ) {
        message(paste0(
          "Downloaded ",
          "(", i, "/", length(missingFilenames), "): ",
          filename
        ))
      }
    }
    
    i <- i + 1
    
  }
  
  return(requestedFilenames)
  
}
