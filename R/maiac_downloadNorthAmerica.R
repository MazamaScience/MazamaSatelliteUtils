#' @title Download North America MAIAC swath data
#' @export
#' @param date desired date (integer, character representing YYYYMMDD[HH] or datetime object)
#' @param julianDate desired date on Julian calendar (YYYYDDD). Ignored if 'date' is specified.
#' @param time UTC hour and minute for data (HHMM). Optional when only one file for the specified date.
#' @param product product code (MAIACAAOT | MAIACABRF | MAIACRTLS | MAIACTAOT | MAIACTBRF)
#' @param tileNumber number code for tile (eg h01v04). 
#' @param baseUrl base URL for data queries
#' @description Download .hdf file of a MAIAC tile for a particular day. 
#' @return 0 if successful.
#' @references \href{http://usfs.airsis.com}{Interagency Real Time Smoke Monitoring}
#' @examples
#' \dontrun{
#' maiac_downloadNorthAmerica( "h01v04", 20171009, 2150 )
#' }

maiac_downloadNorthAmerica <- function(
  tileNumber = NULL,
  date = NULL, 
  time = NULL,
  julianDate = NULL,
  product = "MAIACAAOT",
  baseUrl="https://portal.nccs.nasa.gov/datashare/maiac/DataRelease/NorthAmerica_2000-2016/"
) {
  
  # Sanity check
  if ( is.null(tileNumber) ) {
    stop(paste0("Required parameter 'tileNumber' is missing"))
  }
  
  if ( is.null(date) && is.null(julianDate) ) {
    stop(paste0("'date' or 'julianDate' is required"))
  }
  
  # get dataDir
  satelliteDataDir <- getSatelliteDataDir()
  
  # Get time info
  if ( !is.null(date) ) {
    orders <- c("Ymd", "YmdH", "YmdHM", "YmdHMS")
    datetime <- lubridate::parse_date_time(date, orders, tz = 'UTC')
    year <- as.character(lubridate::year(datetime))
    julianDay <- format(datetime, "%j")
  } else {
    julianDate <- as.character(julianDate)
    if ( stringr::str_length(julianDate) != 7 ) {
      stop(paste0("julianDate must be in format YYYYDDD"))
    }
    year <- julianDate[1:4]
    julianDay <- julianDate[5:7]
  }
  
  
  # Example URL:
  #   https://portal.nccs.nasa.gov/datashare/maiac/DataRelease/NorthAmerica_2000-2016/h01v04/2017/MAIACTBRF.h01v04.20170441955.hdf
  
  
  # Create URL
  url <- paste0(baseUrl, tileNumber, "/", year,"/")
  fileName <- paste0(product, ".", tileNumber, ".", year, julianDay, time)
  
  # Check to see if file exists locally
  localMaiacFiles <- list.files(satelliteDataDir)
  matchMask <- stringr::str_detect(localMaiacFiles, fileName)
  matches <- localMaiacFiles[which(matchMask)]
  matches <- matches[which(stringr::str_detect(matches, ".hdf"))]
  if ( length(matches) > 0 ) {
    if ( length(matches) > 1  ) {
      stop(paste0(">1 matching file: ", paste0(matches, collapse = ", " ), " time must be specified") )
    }
    filePath <- file.path(satelliteDataDir, matches)
  } else {
    # Get list of files at URL
    # Get html of web page
    page <- xml2::read_html(url)
    # Navigate to the correct table
    body <- xml2::xml_child(page, "body")
    table <- xml2::xml_child(body, "div") %>%
      xml2::xml_child("table") %>%
      xml2::xml_child(3) %>% 
      xml2::xml_child(2) %>% 
      xml2::xml_child() %>% 
      xml2::xml_child(5) %>% 
      xml2::xml_child() %>% 
      xml2::xml_child() %>% 
      xml2::xml_child() %>% 
      xml2::xml_child() %>% 
      xml2::xml_child() %>% 
      xml2::xml_child(2) %>% 
      xml2::xml_child() %>% 
      xml2::xml_child() %>% 
      xml2::xml_child(2) 
    
    # Get all link names
    links <- xml2::xml_find_all(table, ".//a") %>% xml2::xml_attr("href")
    
    # Find all from matching date
    matchMask <- stringr::str_detect(links, fileName)
    matches <- links[which(matchMask)]
    if ( length(matches) == 0 ) {
      stop(paste0("No files matching ", fileName, "*"))
    }
    if ( length(matches) > 1  ) {
      stop(paste0(">1 matching file: ", paste0(matches, collapse = ", " ), " time must be specified") )
    }
    
    fileURL <- paste0(url, matches)
    filePath <- file.path(satelliteDataDir, matches)
    
    # Read the url output into a string
    # Only download data if it doesn't already exist
    if ( !file.exists(filePath) ) {
      
      result <- try( utils::download.file(url=fileURL, destfile=filePath) )
      
      # Handle errors
      if ( class(result) != "try-error" ) {
        
        # do nothing on success
        
      } else {
        
        err_msg <- geterrmessage()
        stop(err_msg)
        
      }
      
    }
  }
  
  # return
  return(invisible(filePath))
  
}
