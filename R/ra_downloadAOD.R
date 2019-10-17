#' ra_downloadAOD.R
#' 
#' The purpose of this function is to Download GOES-16 or GOES-17 AOD data for
#' a specific hour, or span of hours.
#' It will first check to see if the files are present locally and if not, will 
#' build a list of the ones that are missing and then download them.
#' 
#' Will flesh this out more if we adopt it.


ra_downloadAOD <- function(
  satID = NULL,
  startTime = NULL,
  endTime = NULL,
  julian = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/",
  timezone = "UTC"
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(startTime)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Must specify GOES satellite ID (G16 or G17)")
  
  if ( !is.null(endTime) ) {
    endTime <- endTime
  }

  # ----- Build the list of files we should have by listing from remote location
  remoteFiles <- ra_listFiles(satID = satID, 
                              startTime = startTime, 
                              endTime = endTime, 
                              timezone = timezone,
                              julian = julian,
                              useRemote = TRUE)
  
  # ---- Build a list of the local files we already have -----------------------
  localFiles <- ra_listFiles(satID = satID, 
                             startTime = startTime, 
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
