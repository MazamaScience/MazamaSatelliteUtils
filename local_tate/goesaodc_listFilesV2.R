goesaodc_listFilesV2 <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  isJulian = FALSE,
  useRemote = FALSE,
  baseUrl = "https://tools-1.airfire.org/Satellite/"
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  
  # Convert satID to uniform case
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) )
    stop("Parameter 'satID' must be either 'G16' (East) or 'G17' (West)")
  
  MazamaCoreUtils::stopIfNull(datetime)
  
  # Validate datetime format
  if ( !lubridate::is.POSIXt(datetime) ) {
    if ( is.na(lubridate::ymd(datetime)) && is.na(lubridate::ymd_h(datetime)) )
      stop("Parameter 'datetime' must be in 'Ymd' or 'Ymd H' format")
  }
  
  # Validate endtime format
  if ( !is.null(endtime) ) {
    if ( !lubridate::is.POSIXt(endtime) ) {
      if ( is.na(lubridate::ymd(endtime)) && is.na(lubridate::ymd_h(endtime)) )
        stop("Parameter 'endtime' must be in 'Ymd' or 'Ymd H' format")
    }
  }
  
  # Use timezone from POSIXt datetime or the given timezone
  if ( lubridate::is.POSIXt(datetime) ) {
    timezone <- lubridate::tz(datetime)
  } else {
    if ( !timezone %in% OlsonNames() )
      stop(sprintf("timezone \"%s\" is not recognized", timezone))
  }
  
  # ----- Prepare data ---------------------------------------------------------
  
  if ( is.null(endtime) ) {
    
    # If datetime specifies hour, set endtime to datetime
    # Otherwise, set endtime to the final hour of datetime day
    if ( !is.na(lubridate::ymd_h(datetime)) ) {
      
      endtime <- MazamaCoreUtils::parseDatetime(
        datetime, 
        timezone = timezone,
        isJulian = isJulian
      )
      
    } else {
      
      endtime <- 
        lubridate::floor_date(
          MazamaCoreUtils::parseDatetime(
            datetime, 
            timezone = timezone, 
            isJulian = isJulian,
          )
        ) +
        lubridate::hours(23) +
        lubridate::minutes(59) +
        lubridate::seconds(59)
      
    }
    
  } else {
    
    # If endtime specifies hour, parse it as a POSIXt
    # Otherwise, set endtime to the final hour of endtime day
    if ( !is.na(lubridate::ymd_h(endtime)) ) {
      
      endtime <- MazamaCoreUtils::parseDatetime(
        endtime, 
        timezone = timezone, 
        isJulian = isJulian,
      )
      
    } else {
      
      endtime <- 
        lubridate::floor_date(
          MazamaCoreUtils::parseDatetime(
            endtime, 
            timezone = timezone, 
            isJulian = isJulian,
          )
        ) +
        lubridate::hours(23) +
        lubridate::minutes(59) +
        lubridate::seconds(59)
      
    }
    
  }
  
  # Parse datetime as POSIXt
  datetime <- MazamaCoreUtils::parseDatetime(
    datetime, 
    timezone = timezone,
    isJulian = isJulian
  )
  
  # Create satUrl for remote searching
  if ( useRemote ) {
    if ( satID == "G16" ) {
      satUrl <- paste0(baseUrl, "GOES-16/AODC")
    } else if ( satID == "G17" ) {
      satUrl <- paste0(baseUrl, "GOES-17/AODC")
    } else {
      stop("Parameter 'satID' must be either 'G16' (East) or 'G17' (West)")
    }
  }
  
  # Build regex pattern
  dataFilePattern <- 
    paste0("OR_ABI-L2-AODC-M[0-9]_", satID, "_s[0-9]+_e[0-9]+_c[0-9]+\\.nc")
  
  # ----- Create a list of files -----------------------------------------------
  
  if ( !useRemote ) {
    
    # Create a list of all appropriate nc files on disk
    dataFiles <- list.files(getSatelliteDataDir(), pattern = dataFilePattern)
    
  } else {
    
    # Check remotely for available files and build filelist
    links <-
      xml2::read_html(satUrl) %>%
      xml2::xml_child("body") %>%
      xml2::xml_child("table") %>%
      xml2::xml_find_all("//a") %>%
      xml2::xml_attr("href")
    
    dataFiles <- links[ -(1:5) ]
    
  }
  
  # Determine the hours covered in the time range
  if ( is.null(endtime) ) {
    hours <- c(datetime)
  } else {
    hours <- seq.POSIXt(from = datetime, to = endtime, by = "hour")
  }
  
  # Create UTC start-patterns for each requested hour
  hourStartPatterns <- c(strftime(hours, "_s%Y%j%H", tz = "UTC"))
  
  # Find files that match hourStartPatterns
  indicesList <- list()
  for ( hourStartPattern in hourStartPatterns ) {
    indicesList[[hourStartPattern]] <- 
      which(stringr::str_detect(dataFiles, hourStartPattern))
  }
  indices <- as.numeric(unlist(indicesList))
  
  # ----- Return ---------------------------------------------------------------
  
  matchingFiles <- dataFiles[indices]
  
  return(matchingFiles)
  
}

if (FALSE) {
  
  library(MazamaSatelliteUtils)
  setSatelliteDataDir("~/Data/Satellite")
  
  # All files for Sept 6
  goesaodc_listFilesV2(
    satID = "G17",
    datetime = "2019-09-06",
    endtime = NULL,
    useRemote = TRUE
  )
  
  # All files for Sept 6 during 6am
  goesaodc_listFilesV2(
    satID = "G17",
    datetime = "2019-09-06 6",
    endtime = NULL,
    useRemote = TRUE
  )
  
  # All files for Sept 6
  goesaodc_listFilesV2(
    satID = "G17",
    datetime = "2019-09-06",
    endtime = "2019-09-06",
    useRemote = TRUE
  )
  
  # All files for Sept 6 and Sept 7
  goesaodc_listFilesV2(
    satID = "G17",
    datetime = "2019-09-06",
    endtime = "2019-09-07",
    useRemote = TRUE
  )
  
  # All files for Sept 6 after (& including) 6am
  goesaodc_listFilesV2(
    satID = "G17",
    datetime = "2019-09-06 6",
    endtime = "2019-09-06",
    useRemote = TRUE
  )
  
  # All files for Sept 6 before (& including) 6am
  goesaodc_listFilesV2(
    satID = "G17",
    datetime = "2019-09-06",
    endtime = "2019-09-06 6",
    useRemote = TRUE
  )
  
  # All files for Sept 6 between (& including) 6am - 8am
  goesaodc_listFilesV2(
    satID = "G17",
    datetime = "2019-09-06 6",
    endtime = "2019-09-06 8",
    useRemote = TRUE
  )
  
}
