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
  
  if ( satID == "G16" ) {
    satUrl <- paste0(baseUrl, "GOES-16/AODC")
  } else if ( satID == "G17" ) {
    satUrl <- paste0(baseUrl, "GOES-17/AODC")
  }
  
  for ( i in 1:length(filenames) ) {
    
    filename <- filenames[i]
    fileUrl <- file.path(satUrl, filename)
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


if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  setSatelliteDataDir("~/Data/Satellite")
  
  goesaodc_downloadScanFiles(
    satID = "G17",
    datetime = "2020-09-08 12:00",
    timezone = "America/Los_Angeles"
  )
  
  goesaodc_downloadScanFiles(
    satID = "G17",
    datetime = "2020-09-08 12:00",
    endtime = "2020-09-08 13:00",
    timezone = "UTC",
    verbose = TRUE
  )
  
}