#' @export
#' @title download GOES 16 AOD data
#' @param date desired date (integer, character representing YYYYMMDD or datetime object)
#' @param hour UTC hour for data (HH)
#' @param julianDate desired date on Julian calendar (YYYYDDD). Ignored if 'date' is specified.
#' @param product desired data product. Currently, only 'AODC' is supported.
#' @param baseUrl base URL for data queries
#' @return 0 if successful
#' 

# for testing
if (FALSE) {
  date = 20190415
  hour = 11
  julianDate = 2019105
  product = "AODC"
  baseUrl = "https://tools-1.airfire.org/Satellite/GOES-16"
  downloadedFiles <- goes_downloadAOD(date = date, 
                                      julianDate = julianDate, 
                                      hour = hour, 
                                      product = product)
  downloadedFiles
}

goes_downloadAOD <- function(
  date = NULL,
  hour = NULL,
  julianDate = NULL,
  product = "AODC",
  baseUrl = "https://tools-1.airfire.org/Satellite/GOES-16"
) {
  # ----- input validation -----------------------------------------------------
  
  # required parameters are provided
  if (is.null(date) & is.null(julianDate)) {
    stop(paste0("'date' or 'julianDate' is required"))
  }
  
  if (is.null(hour)) {
    stop(paste0("'hour' is required"))
  }
  
  # parameters are of correct format
  time <- as.character(hour)
  if (stringr::str_length(hour) != 2) {
    stop(paste0("'hour' must be of the format HH"))
  }
  
  if (is.POSIXct(date)) {
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
  
  validProducts <- c("AODC")
  if (!(product %in% validProducts)) {
    stop(paste0("Invalid value for 'product' parameter. Please choose from the following: ",
                paste0(validProducts, collapse = ", ")))
  }
  
  # ----- Download data --------------------------------------------------------
  
  # Parse date and time
  if (!is.null(date)) {
    orders <- c("YmdH")
    datetime <- lubridate::parse_date_time(paste0(date, hour), orders=orders, tz="UTC")
  } else {
    orders <- c("YjH")
    datetime <- lubridate::parse_date_time(paste0(julianDate, hour), orders=order, tz="UTC")
  }
  
  # Create URL
  url <- paste(baseUrl, product, sep="/")
  
  # Get list of available files for specified date
  links <- 
    xml2::read_html(url) %>%
    xml2::xml_child("body") %>% 
    xml2::xml_child("table") %>%
    xml2::xml_find_all("//a") %>%
    xml2::xml_attr("href")
  
  availableFiles <- links[-(1:5)]
  matchingFiles <- stringr::str_subset(availableFiles, format(datetime, "%Y%j%H"))
  
  # Get satelliteDataDir
  satelliteDataDir <- getSatelliteDataDir()
  
  # Download matches
  downloadedFiles <- NULL
  for (file in matchingFiles) {
    filePath <- paste0(satelliteDataDir,"/", file)
    if (!file.exists(filePath)) { # don't download if file exists locally
      fileUrl <- paste0(url, "/", file)
      
      result <- try({
        utils::download.file(fileUrl, destfile = filePath)
        downloadedFiles <- c(downloadedFiles, filePath)
        }, silent=FALSE)
      
      if ("try-error" %in% class(result)) {
        errMsg <- geterrmessage()
        stop(errMsg)
      }
    }
  }
  
  return(invisible(downloadedFiles))
}
