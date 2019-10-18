#' @export
#'
#' @title List available GOES AOD files for a specified date and hour
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Desired datetime in any Ymd H [MS] format or \code{POSIXct}
#' @param endTime Desired ending time in any Ymd H [MS] format or \code{POSIXct}
#' @param useRemote Logical specifying whether to look for files in 
#' \code{getSatelliteDataDir()} or \code{baseUrl}.
#' @param timezone Timezone used to interpret \code{datetime} and \code{endTime}
#' @param julian Logical specifying if \code{datetime} (and optionally 
#' \code{endTime}) are Julian formatted
#' @param baseUrl Base URL for data queries.
#' 
#' 
#' @description Retrieve a list of GOES AOD files available in the
#' \code{satelliteDataDir} or at \code{baseUrl} for a specified date and hour.
#'
#' NOTE: All files for a particular hour will be returned even if the
#' incoming \code{startdate} or \code{jdate} is specified to the minute or
#' second.  By default, the local directory set by \code{SetSatelliteDir} is 
#' searched. \code{useRemote = TRUE} will search \code{baseURL} for all files 
#' that are available within the specified timeframe.
#'
#' @return Vector of filenames.
#'
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#'
#'  goesaodc_listFiles(
#'  satID = "G16",
#'  datetime = "2019-09-06 06:00",
#'  endTime = "2019-09-06 18:00",
#'  timezone = "America/Los_Angeles",
#'  useRemote = TRUE,
#'  )
#' }

goesaodc_listFiles <- function(
  satID = NULL,
  datetime = NULL,
  endTime = NULL,
  useRemote = FALSE,
  timezone = "UTC",
  julian = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ---- Validate we have what we need -----------------------------------------
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(datetime)
  
  # ---- Convert satID to uniform case -----------------------------------------
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Must specify GOES satellite ID (G16 or G17)")
  
  # ---- Check if a timezone has been passed in with a POSIXt ------------------
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(datetime)[1] %in% time_classes ) {
    timezone <- attr(datetime, "tzone")
  }
  
  # ---- Parse incoming times with MazamaCoreUtils -----------------------------
  datetime <- MazamaCoreUtils::parseDatetime(datetime = datetime, 
                                             timezone = timezone, 
                                             julian = julian)
  
  if ( !is.null(endTime) ) {
   endTime <- MazamaCoreUtils::parseDatetime(endTime, timezone, julian = julian)
  }
  
  # ---- Create satUrl for remote searching
  if ( useRemote ) {
    if ( satID == "G16" ) {
      satUrl <- paste0(baseUrl, "GOES-16/AODC")
    } else if ( satID == "G17" ) {
      satUrl <- paste0(baseUrl, "GOES-17/AODC")
    } else {
      stop("Parameter 'satID' must be either 'G16' or 'G17'")
    }
  }
  
  # ----- Assemble regex to search with ----------------------------------------
  regex <- paste0( "OR_ABI-L2-AODC-M[0-9]_",
                   satID,
                   "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  )
  
  # ---- Create a list of all appropriate nc files on disk ---------------------
  if ( !useRemote) {
    dataFiles <- list.files(getSatelliteDataDir(), pattern = regex)
    
  # ---- Check remotely for available files and build filelist -----------------
  } else {
    
    links <-
      xml2::read_html(satUrl) %>%
      xml2::xml_child("body") %>%
      xml2::xml_child("table") %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")
    
    dataFiles <- links[ -(1:5) ]
  }
  
  # ----- Build a squence of hours ---------------------------------------------
  if ( is.null(endTime) ) {
    hours <- c(datetime)
  } else {
    hours <- seq.POSIXt(from = datetime, to = endTime, by = "hour")
  }
  
  # Convert to UTC and create startPatterns for each requested hour
  startPatterns <- c(strftime(hours, "_s%Y%j%H", tz = "UTC"))
  
  # ---- Match startPatterns to list of filenames ------------------------------
  indicesList <- list()
  for ( startPattern in startPatterns ) {
    indicesList[[startPattern]] <- 
      which(stringr::str_detect(dataFiles, startPattern))
  }
  indices <- as.numeric(unlist(indicesList))
  
  matchingFiles <- dataFiles[indices]
  
  return(matchingFiles)
  
} # END OF FUNCTION

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  satID <- "G16"
  datetime <- "2019-09-06 06:00"
  endTime <- "2019-09-06 18:00"
  timezone <- "America/Los_Angeles"
  useRemote <- TRUE
  baseUrl <- "https://tools-1.airfire.org/Satellite/"
  
  goesaodc_listFiles(
    satID = satID,
    datetime = datetime,
    endTime = endTime,
    timezone = timezone,
    useRemote = useRemote,
    baseUrl = baseUrl) 
} 
