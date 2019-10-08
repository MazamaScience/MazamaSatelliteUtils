#' @export
#'
#' @title Download GOES-16 or GOES-17 AOD data
#'
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime desired date in any Ymd [H] format or \code{POSIXct}
#' @param jdate desired date in as a Julian date string, i.e. as seen in the
#'   netcdf filenames
#' @param baseUrl base URL for data queries
#' @param quiet if TRUE, suppress status messages and progress bar
#' @param fullDay if TRUE downloads all files for a day, even if a specific hour
#' has been specified in the \code{datetime}. Set to FALSE by default.
#'
#' @description Download all GOES 16 or 17 NetCDF files for the given
#' \code{datetime} to the directory specified by \code{setSatelliteDataDir()}.
#' If \code{datetime} is specified to the hour and \code{fullDay} is not
#' explicitly set to TRUE', only files for that hour will be downloaded. If
#' \code{datetime} is specified only to the day, all files for that day will
#' be downloaded. NOTE that all times are assumed to be in UTC timezone.
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
#' goesaodc_downloadAOD(satID ="G16", datetime = "2019-05-16 16")
#' 
#' goesaodc_downloadAOD(satID = "G17", jdate = "201924918")
#' }

goesaodc_downloadAOD <- function(
  satID = NULL,
  datetime = NULL,
  jdate = NULL,
  baseUrl = "https://tools-1.airfire.org/Satellite/",
  quiet = FALSE,
  fullDay = FALSE
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Must specify GOES satellite ID (G16 or G17)")

   # IF A datetime HAS BEEN PASSED IN, ATTEMPT TO PARSE IT
  if ( !is.null(datetime) ) {
  
    suppressWarnings(
      starttime <- MazamaCoreUtils::parseDatetime(datetime, timezone = "UTC") )
    if (lubridate::hour(starttime) == 0) {
      fullDay <- TRUE
    }
    
  # OTHERWISE IF jdate PRESENT, CONVERT IT TO POSIXt
  } else if ( !is.null(jdate) ) {
  
    # sample = 20192491646196
    # JDATE IS STRIPPED TO 13 CHARS AS 14TH WON'T PARSE
    jdate <- stringr::str_sub(jdate, 1, 13)
    formats <- c("Yj", "YjH", "YjHMS")
    starttime <- lubridate::parse_date_time(jdate, orders = formats, tz = "UTC")
    
    } else {
    
    stop("Either 'datetime' or 'jdate' must be defined.", call. = FALSE)
    
    }
    
  # CONVERT starttime TO CORRECT JULIAN FORMAT BASED ON 'fullDay' PARAMETER
  if ( fullDay ) {
  
    startString <- strftime(starttime, "%Y%j", tz = "UTC")
    startString <- stringr::str_sub(startString, 1, 7)
    
  } else {
  
    startString <- strftime(starttime, "%Y%j%H", tz = "UTC")
    startString <- stringr::str_sub(startString, 1, 9)
    
  }
  
  # Choose satellite data source directory
  if (satID == "G16") {
  
    satUrl <- paste0(baseUrl, "GOES-16/AODC")
    
  } else if (satID == "G17") {
  
    satUrl <- paste0(baseUrl, "GOES-17/AODC")
  }
  
  # ----- Download Data --------------------------------------------------------
  
  # Get list of available files for specified date
  links <-
    xml2::read_html(satUrl) %>%
    xml2::xml_child("body") %>%
    xml2::xml_child("table") %>%
    xml2::xml_find_all("//a") %>%
    xml2::xml_attr("href")
  
  availableFiles <- links[-(1:5)]
  
  # get the scan start times from files found at baseUrl
  startTimes <- purrr::map_chr(availableFiles, goesaodc_getStartString)
  
  # select matching files
  mask <- stringr::str_detect(startTimes, startString)
  matchingFiles <- availableFiles[mask]
  
  # Get satelliteDataDir
  satelliteDataDir <- getSatelliteDataDir()
  
  # Download matches
  downloadedFiles <- NULL
  for ( file in matchingFiles ) {
    filePath <- paste0(satelliteDataDir, "/", file)
    # don't download if file exists locally
    if ( !file.exists(filePath) ) {
      fileUrl <- paste0(satUrl, "/", file)
      
      result <- try({
        utils::download.file(fileUrl, destfile = filePath, quiet = quiet)
        downloadedFiles <- c(downloadedFiles, filePath)
      }, silent = FALSE)
      
    # if ( "try-error" %in% class(result) ) {
    #    errMsg <- geterrmessage()
    # TODO:  logger.warn(errMsg)
    # Start work on the next file ###stop(errMsg)
    # }
    }
  }
  
  return(invisible(downloadedFiles))
  
}
