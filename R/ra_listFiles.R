#' @export
#' 
#' @title List available GOES AOD files for a specified date and hour
#' 
#' Inputs:
#' @param satID: G16 or G17
#' @param startTime: Desired hour to search for
#' @param endTime: Optional ending hour, used to specify a time range
#' @param useRemote: (optional) Specify whether to look for files on remote server
#' @param julian: (optional) Specify if incoming times are in Julian format
#' @param baseUrl: (optional) Base URL for remote data queries.
#' 
#' @description List available GOES AOD files for a specified date and hour.  Will 
#' check both locally and (optionally) on remote server. Will also check for a 
#' range of hours.  Will accept any time format MazamaCoreUtils can parse.  If
#' given the optional \code{endTime} parameter, will build a list of files that 
#' spans the period between \code{startTime} and \code{endTime}.  It will respect
#' any valid timezone that is passed in, but will default to UTC if none is there.
#' 
#' Note that all files for a particular hour will be returned even if the
#' incoming \code{startdate} or \code{jdate} is specified to the minute or
#' second.
#' 
#' 
#' @return Vector of filenames.
 

ra_listFiles <- function(
  satID = NULL,
  startTime = NULL,
  endTime = NULL,
  useRemote = FALSE,
  timezone = "UTC",
  julian = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ---- Validate we have what we need -----------------------------------------
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(startTime)
  
  # ---- Convert satID to uniform case -----------------------------------------
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Must specify GOES satellite ID (G16 or G17)")
  
  # ---- Check if a timezone has been passed in with a POSIXt ------------------
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(startTime)[1] %in% time_classes ) {
    timezone <- attr(datetime,"tzone")
  }
  
  # ---- Parse incoming times with MazamaCoreUtils -----------------------------
  startTime <- MazamaCoreUtils::parseDatetime(datetime = startTime, 
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
    
  # ----- Check remotely for available files and build filelist ----------------  
  } else {
    
    links <-
      xml2::read_html(satUrl) %>%
      xml2::xml_child("body") %>%
      xml2::xml_child("table") %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")
    
    dataFiles <- links[-(1:5)]
  }
  
  # ----- Build a squence of hours ---------------------------------------------
  if ( is.null(endTime) ) {
    hours <- c(startTime)
  } else {
    hours <- seq.POSIXt(from = startTime, to = endTime, by = "hour")
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


