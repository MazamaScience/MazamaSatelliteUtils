#' @export
#'
#' @title Download GOES AOD data for a specific date
#'
#' @description Downloads a GOES satellite's NetCDF files for the given 
#' \code{datetime} to the directory specified by \code{setSatelliteDataDir()}.
#' If \code{endtime} is specified, all files from \code{startime} up to
#' (but not including) \code{endtime} will be downloaded. Otherwise, only files 
#' for the \code{datetime} hour will be retrieved.
#' 
#' The vector of files returned includes all files in \code{satelliteDataDir}
#' that match the requested date whether they were download previously or with
#' this call.
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param endtime End time in any Ymd H [MS] format or \code{POSIXct} 
#' (exclusive).
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param isJulian Logical value determining whether \code{datetime} should be 
#' interpreted as a Julian date with day of year as a decimal number; Defaults 
#' to FALSE.
#' @param verbose Logical flag to print download progress; Defaults to FALSE.
#' @param baseUrl URL of remote database; Defaults to 
#' "https://tools-1.airfire.org/Satellite/".
#'
#' @return Invisibly returns a vector of local files matching the requested 
#' \code{datetime}.
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
#' # Time range
#' files <- goesaodc_downloadAOD(
#'   satID = "G16",
#'   datetime = "2019-09-06 08:00",
#'   endtime = "2019-09-06 12:00",
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
  endtime = NULL,
  timezone = "UTC",
  isJulian = FALSE,
  verbose = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Must specify GOES satellite ID (G16 or G17)")
  
  if ( !is.null(endtime) ) {
    endtime <- endtime
  }
  
  # ----- Determine files to download ------------------------------------------
  
  # List of files available from the remote location
  remoteFiles <- goesaodc_listFiles(
    satID = satID, 
    datetime = datetime, 
    endtime = endtime, 
    timezone = timezone,
    isJulian = isJulian,
    useRemote = TRUE
  )
  
  # Stop if there are no files available for this time
  if ( rlang::is_empty(remoteFiles) )
    stop("There is no data available for this time.")

  # List of local files
  localFiles <- goesaodc_listFiles(
    satID = satID, 
    datetime = datetime, 
    endtime = endtime, 
    timezone = timezone,
    isJulian = isJulian,
    useRemote = FALSE
  )
  
  # Files to download
  missingFiles <- setdiff(remoteFiles, localFiles)
  
  # ----- Download missing files -----------------------------------------------
  
  satelliteDataDir <- getSatelliteDataDir()
  
  if ( satID == "G16" ) {
    satUrl <- paste0(baseUrl, "GOES-16/AODC")
  } else if ( satID == "G17" ) {
    satUrl <- paste0(baseUrl, "GOES-17/AODC")
  }
  
  i <- 1
  for ( file in missingFiles ) {
    
    filePath <- file.path(satelliteDataDir, file)
    fileUrl <- paste0(satUrl, "/", file)
    
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
        message(paste0("Downloaded (", i, "/", length(missingFiles), "): ", file))
      }
    }
    
    i <- i + 1
    
  }

  # ----- Return ---------------------------------------------------------------
  
  # Updated list of local files
  localFiles <- goesaodc_listFiles(
    satID = satID, 
    datetime = datetime, 
    endtime = endtime, 
    timezone = timezone,
    isJulian = isJulian,
    useRemote = FALSE
  )
  
  return(invisible(localFiles))
  
}
