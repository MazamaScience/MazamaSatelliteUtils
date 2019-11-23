#' @export
#'
#' @title Download GOES-16 or GOES-17 AOD data
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Desired datetime in any Ymd H [MS] format or \code{POSIXct}
#' @param endTime Desired ending time in any Ymd H [MS] format or \code{POSIXct}
#' @param timezone Timezone used to interpret \code{datetime} and \code{endTime}
#' @param isJulian Logical specifying if \code{datetime} (and optionally 
#' \code{endTime}) are Julian formatted
#' @param verbose Logical parameter specifying whether download status should be
#'  shown.
#' @param baseUrl Base URL for data queries.
#'
#' @description Download all GOES 16 or 17 NetCDF files for the given
#' \code{datetime} to the directory specified by \code{setSatelliteDataDir()}.
#' If \code{datetime} is specified to the hour and \code{fullDay} is not
#' explicitly set to TRUE', only files for that hour will be downloaded. If the
#' optional \code{endTime} is specified, all files that exist for the time 
#' period between \code{datetime} and \code{endTime} will be downloaded. If 
#' \code{timezone} is not specified, "UTC" is assumed.
#' 
#' The vector of files returned includes all files in \code{satelliteDataDir}
#' that match the requested date whether they were download previously or with
#' this call.
#'
#' @return Invisibly returns a vector of local files matching the requested \code{datetime}.
#'
#' @seealso \code{\link{setSatelliteDataDir}}
#'
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' # Single hour
#' files <- goesaodc_downloadAOD("G17", "2019-09-06 18") 
#' print(files)
#' 
#' # Specific time range
#' files <- goesaodc_downloadAOD(
#'   satID = "G16",
#'   datetime = "2019-09-06 08:00",
#'   endTime = "2019-09-06 12:00",
#'   timezone = "America/Los_Angeles"
#' )
#' print(files)
#' 
#' # Julian timestamp
#' files <- goesaodc_downloadAOD(
#'   satID = "G16",
#'   datetime = "201924915",
#'   isJulian = TRUE
#' )
#' print(files)
#' }

goesaodc_downloadAOD <- function(
  satID = NULL,
  datetime = NULL,
  endTime = NULL,
  timezone = "UTC",
  isJulian = FALSE,
  verbose = FALSE,
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
  
  # ----- Determine files to download ------------------------------------------
  
  # List of files available from the remote location
  remoteFiles <- goesaodc_listFiles(
    satID = satID, 
    datetime = datetime, 
    endTime = endTime, 
    timezone = timezone,
    isJulian = isJulian,
    useRemote = TRUE
  )
  
  # Stop if there are no files available for this time
  if ( rlang::is_empty(remoteFiles) )
    stop("There is no data available for this time.")

  # Stop if more than 24 hrs of data are requested
  if ( length(remoteFiles) > 288 )
    stop("More than 24 hours of data requested.")

  # List of local files
  localFiles <- goesaodc_listFiles(
    satID = satID, 
    datetime = datetime, 
    endTime = endTime, 
    timezone = timezone,
    isJulian = isJulian,
    useRemote = FALSE
  )
  
  # Files to download
  missingFiles <- setdiff(remoteFiles, localFiles)
  
  # ---- Download missing files ------------------------------------------------
  
  satelliteDataDir <- getSatelliteDataDir()
  
  if ( satID == "G16" ) {
    satUrl <- paste0(baseUrl, "GOES-16/AODC")
  } else if ( satID == "G17" ) {
    satUrl <- paste0(baseUrl, "GOES-17/AODC")
  }
  
  for ( file in missingFiles ) {
    
    filePath <- file.path(satelliteDataDir, file)
    fileUrl <- paste0(satUrl, "/", file)
    
    result <- try({
      utils::download.file(fileUrl, 
                           destfile = filePath, 
                           quiet = TRUE, 
                           method = "auto", 
                           mode = "wb")
    }, silent = FALSE)
    
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      if ( MazamaCoreUtils::logger.isInitialized() ) {
        MazamaCoreUtils::logger.warn(err_msg)
      }
    } else {
      if (verbose == TRUE) {
        message(paste0("Downloaded ", file))
      }
    }
    
  }

  # ----- Return ---------------------------------------------------------------
  
  # Updated list of local files
  localFiles <- goesaodc_listFiles(
    satID = satID, 
    datetime = datetime, 
    endTime = endTime, 
    timezone = timezone,
    isJulian = isJulian,
    useRemote = FALSE
  )
  
  return(invisible(localFiles))
  
}
