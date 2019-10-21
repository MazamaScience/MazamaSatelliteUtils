#' @export
#'
#' @title Download GOES-16 or GOES-17 AOD data
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Desired datetime in any Ymd H [MS] format or \code{POSIXct}
#' @param endTime Desired ending time in any Ymd H [MS] format or \code{POSIXct}
#' @param timezone Timezone used to interpret \code{datetime} and \code{endTime}
#' @param julian Logical specifying if \code{datetime} (and optionally 
#' \code{endTime}) are Julian formatted
#' @param baseUrl Base URL for data queries.
#'
#' @description Download all GOES 16 or 17 NetCDF files for the given
#' \code{datetime} to the directory specified by \code{setSatelliteDataDir()}.
#' If \code{datetime} is specified to the hour and \code{fullDay} is not
#' explicitly set to TRUE', only files for that hour will be downloaded. If the
#' optional \code{endTime} is specified, all files that exist for the time 
#' period between \code{datetime} and \code{endTime} will be downloaded. If 
#' \code{timezone} is not specified, "UTC" timezone is assumed.
#'
#' @return Vector of downloaded filepaths.
#'
#' @seealso \code{\link{setSatelliteDataDir}}
#'
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' goesaodc_downloadAOD(satID = "G17", 
#' datetime = "2019-09-06 18") 
#' 
#' goesaodc_downloadAOD(satID = "G16",
#' datetime = "2019-09-06 08:00",
#' endTime = "2019-09-06 12:00",
#' timezone = "America/Los_Angeles")
#' 
#' goesaodc_downloadAOD(satID = "G16",
#' datetime = "201924915",
#' julian = TRUE)
#'
#' }

goesaodc_downloadAOD <- function(
  satID = NULL,
  datetime = NULL,
  endTime = NULL,
  timezone = "UTC",
  julian = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ---- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Must specify GOES satellite ID (G16 or G17)")
  
  if ( !is.null(endTime) ) {
    endTime <- endTime
  }
  
  # ----- Build the list of files we should have by listing from remote location
  remoteFiles <- goesaodc_listFiles(satID = satID, 
                              datetime = datetime, 
                              endTime = endTime, 
                              timezone = timezone,
                              julian = julian,
                              useRemote = TRUE)
  
  # ---- Force a stop() if there are no files available for this time ----------
  if (rlang::is_empty(remoteFiles)) {
    stop("There is no data available for this time.")
  }
  
  
  # ---- Build a list of the local files we already have -----------------------
  localFiles <- goesaodc_listFiles(satID = satID, 
                             datetime = datetime, 
                             endTime = endTime, 
                             timezone = timezone,
                             julian = julian,
                             useRemote = FALSE)
  
  # ---- Create the offset by set comparison of the 2 sets of files ------------
  missingFiles <- setdiff(remoteFiles, localFiles)
  
  # ---- Build the URLs for the missing files ----------------------------------
  if (satID == "G16") {
    satUrl <- paste0(baseUrl, "GOES-16/AODC")
    
  } else if (satID == "G17") {
    satUrl <- paste0(baseUrl, "GOES-17/AODC")
  }
  
  # ---- Download missing files ------------------------------------------------
  satelliteDataDir <- getSatelliteDataDir()
  
  downloadedFiles <- NULL
  for ( file in missingFiles ) {
    filePath <- file.path(satelliteDataDir, file)
    fileUrl <- paste0(satUrl, "/", file)
    result <- try({
      utils::download.file(fileUrl, 
                           destfile = filePath, 
                           quiet = TRUE, 
                           method = "auto", 
                           mode = "wb")
      downloadedFiles <- c(downloadedFiles, filePath)
    }, silent = FALSE)
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      if ( MazamaCoreUtils::logger.isInitialized() ) 
        MazamaCoreUtils::logger.warn(err_msg)
    } else {
      print(paste0("Downloaded ", file))
    }
  }
  
  return(invisible(downloadedFiles))
  
}
