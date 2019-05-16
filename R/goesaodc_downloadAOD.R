#' @export
#' 
#' @title Download GOES 16 AOD data
#' 
#' @param date desired date (integer, character representing YYYYMMDD or \code{POSIXct})
#' @param hour UTC hour for data (HH)
#' @param julianDate desired date on Julian calendar (YYYYDDD). Ignored if \code{date} is specified.
#' @param baseUrl base URL for data queries
#' 
#' @description Download all GOES 16 .nc files for the given date and hour to
#' the directory specified by \code{setSatelliteDataDir()}.
#' 
#' @return Vector of downloaded filepaths.
#' 
#' @seealso \code{\link{setSatelliteDataDir}}


goesaodc_downloadAOD <- function(
  date = NULL,
  hour = NULL,
  julianDate = NULL,
  baseUrl = "https://tools-1.airfire.org/Satellite/GOES-16/AODC"
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  # required parameters are provided
  if ( is.null(date) && is.null(julianDate) ) {
    stop("Required parameter 'date' or 'julianDate' is missing")
  }
  
  if ( is.null(hour) ) {
    stop(paste0("Required parameter 'hour' is missing"))
  }
  
  # parameters are of correct format
  time <- as.character(hour)
  if (stringr::str_length(hour) != 2) {
    stop(paste0("'hour' must be of the format HH"))
  }
  
  if (lubridate::is.POSIXct(date)) {
    date <- format(date, "%Y%m%d")
  } else {
    date <- as.character(date)
    if (stringr::str_length(date) != 8) {
      stop(paste0("'date' must be of the format YYYYMMDD"))
    }
  }
  
  if (!is.null(julianDate)) {
    julianDate <- as.character(julianDate)
    if (stringr::str_length(julianDate) != 7) {
      stop(paste0("'julianDate' must be of the format YYYYDDD"))
    }
  }
  
  # ----- Download Data --------------------------------------------------------
  
  # Parse date and time
  if ( !is.null(date) ) {
    orders <- c("YmdH")
    datetime <- lubridate::parse_date_time(paste0(date, hour), orders=orders, tz="UTC")
  } else {
    orders <- c("YjH")
    datetime <- lubridate::parse_date_time(paste0(julianDate, hour), orders=order, tz="UTC")
  }
  
  # Get list of available files for specified date
  links <- 
    xml2::read_html(baseUrl) %>%
    xml2::xml_child("body") %>% 
    xml2::xml_child("table") %>%
    xml2::xml_find_all("//a") %>%
    xml2::xml_attr("href")
  
  availableFiles <- links[-(1:5)]
  
  # get the scan start times from files found at baseUrl
  startTimes <- purrr::map_chr(availableFiles, goesaodc_getStartString)
  
  # select matching files
  mask <- stringr::str_detect(startTimes, format(datetime, "%Y%j%H"))
  matchingFiles <- availableFiles[mask]
  
  # Get satelliteDataDir
  satelliteDataDir <- getSatelliteDataDir()
  
  # Download matches
  downloadedFiles <- NULL
  for ( file in matchingFiles ) {
    filePath <- paste0(satelliteDataDir,"/", file)
    # don't download if file exists locally
    if ( !file.exists(filePath) ) {
      fileUrl <- paste0(baseUrl, "/", file)
      
      result <- try({
        utils::download.file(fileUrl, destfile = filePath)
        downloadedFiles <- c(downloadedFiles, filePath)
      }, silent=FALSE)
      
      if ( "try-error" %in% class(result) ) {
        errMsg <- geterrmessage()
        # TODO:  logger.warn(errMsg)
        # Start work on the next file ###stop(errMsg)
      }
    }
  }
  
  return(invisible(downloadedFiles))
  
}


# ===== DEBUGGING ==============================================================

if (FALSE) {
  
  date <- 20190422
  hour <- "09"
  downloadedFiles <- goesaodc_downloadAOD(date = date, 
                                      hour = hour)
  
  print(downloadedFiles)
  
}
