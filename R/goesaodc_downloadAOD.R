#' @export
#' 
#' @title Download GOES 16 AOD data
#' 
#' @param startdate desired date in any Y-m-d [H] format or \code{POSIXct}
#' @param jdate desired date in as a Julian date string, i.e. as seen in the
#'   netcdf filenames
#' @param baseUrl base URL for data queries
#' @param quiet if TRUE, suppress status messages and progress bar
#' 
#' @description Download all GOES 16 NetCDF files for the given \code{startdate} 
#' to the directory specified by \code{setSatelliteDataDir()}. If 
#' \code{startdate} is specified to the hour, only files for that hour will be 
#' downloaded. If \code{startdate} is specified only to the day, all files for 
#' that day will be downloaded.
#' 
#' @return Vector of downloaded filepaths.
#' 
#' @seealso \code{\link{setSatelliteDataDir}}


goesaodc_downloadAOD <- function(
  startdate = NULL,
  jdate = NULL,
  baseUrl = "https://tools-1.airfire.org/Satellite/GOES-16/AODC",
  quiet = FALSE
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  if ( !is.null(startdate) ) {
    
    # Is it a full day?
    suppressWarnings({
      starttime <- lubridate::parse_date_time(startdate, "Ymd", tz = "UTC") 
    })
    if ( !is.na(starttime) ) {
      fullDay <- TRUE
    } else {
      orders <- c("YmdH","YmdHM","YmdHMS")
      suppressWarnings({
        starttime <- lubridate::parse_date_time(startdate, orders, tz = "UTC")
      })
      if ( is.na(starttime) ) {
        stop("Parameter 'startdate' cannot be interpreted. Is it a 'jdate'?")
      }
      fullDay <- FALSE
    }
    
  } else if ( !is.null(jdate) ) {
    
    # Check for operator error
    if ( !is.numeric(jdate) && !is.character(jdate) ) {
      jdate_class <- paste(class(jdate), sep = ", ")
      stop(paste0("Parameter 'jdate' cannot be of class '", jdate_class, "'"), call. = FALSE)
    }
    
    jdate <- as.character(jdate)
    
    # Check for operator error
    if ( stringr::str_detect(jdate, "[^0-9]") ) {
      stop(paste0("'", jdate, "' is not a Julian date string."), call. = FALSE)
    }
    
    if ( stringr::str_count(jdate) == 5 ) {
      starttime <- strptime(jdate, "%Y%j", tz = "UTC")
      fullDay <- TRUE
    } else if ( stringr::str_count(jdate) == 7 ) {
      starttime <- strptime(jdate, "%Y%j%H", tz = "UTC")
      fullDay <- FALSE
    } else if ( stringr::str_count(jdate) == 9 ) {
      starttime <- strptime(jdate, "%Y%j%H", tz = "UTC")
      fullDay <- FALSE
    } else {
      # strip the string down to the YjH level
      jdate <- stringr::str_sub(jdate, end = 9)
      starttime <- strptime(jdate, "%Y%j%H", tz = "UTC")
      fullDay <- FALSE
    }
    
  } else {
    
    stop("Either 'startdate' or 'jdate' must be defined.", call. = FALSE)
    
  }
  
  # Julian string for comparison with file names
  if ( fullDay ) {
    startString <- strftime(starttime, "%Y%j", tz = "UTC")
  } else {
    startString <- strftime(starttime, "%Y%j%H", tz = "UTC")
  }
  
  # ----- Download Data --------------------------------------------------------
  
  # DELETE ME: POSIXct called "datetime" by here
  
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
  mask <- stringr::str_detect(startTimes, startString)
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
        utils::download.file(fileUrl, destfile = filePath, quiet = quiet)
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
  
  setSatelliteDataDir("~/Data/Satellite")
  
  # Hour where startdate is numeric
  startdate <- 2019051616
  downloadedFiles <- goesaodc_downloadAOD(startdate)
  
  # Full day where startdate is POSIXct
  startdate <- lubridate::parse_date_time("20190516", "Ymd", tz = "UTC")
  downloadedFiles <- goesaodc_downloadAOD(startdate)
}
