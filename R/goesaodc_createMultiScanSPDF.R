#' @title Create spatial points for multiple scan
#' 
#' @description Creates a SpatialPointsDataFrame of averaged AOD values across
#' multiple scans.
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param endtime End time in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level; Defaults to 3.

goesaodc_createMultiScanSPDF <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  bbox = bbox_CONUS,
  dqfLevel = 3
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # ----- Get scan files -------------------------------------------------------
  
  startFilename <- goasaodc_findClosestScanFile(
    satID = satID,
    datetime = datetime,
    timezone = timezone
  )
  
  endFilename <- goasaodc_findClosestScanFile(
    satID = satID,
    datetime = endtime,
    timezone = timezone
  )
  
  satUrl <- paste0(
    "https://tools-1.airfire.org/Satellite/",
    ifelse(satID == "G16", "GOES-16/AODC/", "GOES-17/AODC/")
  )
  
  # Get all remote netCDF files
  links <-
    xml2::read_html(satUrl) %>%
    xml2::xml_child("body") %>%
    xml2::xml_child("table") %>%
    xml2::xml_find_all("//a") %>%
    xml2::xml_attr("href")
  
  allScanFiles <- links[ -(1:5) ]
    
  startIndex <- which(startFilename == allScanFiles)[1]
  endIndex <- which(endFilename == allScanFiles)[1] # -1 for exclusive end bound?
  
  filenames <- allScanFiles[startIndex:endIndex]
  
  # For each required scan file
  for ( filename in filenames ) {
    
    # Download the file if it isn't available locally
    if ( !(filename %in% list.files(getSatelliteDataDir())) ) {
      
      satUrl <- ifelse(satID == "G16", "GOES-16/AODC/", "GOES-17/AODC/")
      
      fileUrl <- paste0(
        "https://tools-1.airfire.org/Satellite/",
        satUrl,
        filename
      )
      
      filePath <- file.path(getSatelliteDataDir(), filename)
      
      utils::download.file(
        fileUrl,
        destfile = filePath, 
        quiet = TRUE, 
        method = "auto", 
        mode = "wb"
      )
      
    }
    
  }
  
  # ----- Create netCDF handles ------------------------------------------------
  
  ncList <- list()
  
  for ( filename in filenames ) {
    
    label <- 
      filename %>%
      goesaodc_convertFilenameToDatetime() %>%
      MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
    
    ncList[[label]] <- goesaodc_openFile(basename(filename))
    
  }
  
  # ----- Create tibbles -------------------------------------------------------
  
  tbList <- list()
  
  for ( nc in ncList ) {
    
    label <- 
      basename(nc$filename) %>%
      goesaodc_convertFilenameToDatetime() %>%
      MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
    
    # Do not drop NA rows, since we want to keep all scan tibbles the same size
    tbList[[label]] <- goesaodc_createTibble(
      nc = nc,
      bbox = bbox,
      dropNa = FALSE
    )
    
  }
  
  # ----- Average the scan readings --------------------------------------------
  
  pointsCount <- nrow(tbList[[1]])
  
  # Define a tibble of average AOD values. Use coords from the first scan since 
  # each scan *should* have the same sample locations
  avgTb <- tibble::tibble(
    lon = tbList[[1]]$lon,
    lat = tbList[[1]]$lat,
    AOD = rep(0, pointsCount)
  )
  
  # For each sample location
  for ( p in seq(pointsCount) ) {
    
    # Gather each scan's AOD reading at that location 
    pointValues <- rep(0, length(tbList))
    for ( tb in seq(length(tbList)) ) {
      pointValues[tb] <- tbList[[tb]]$AOD[p]
    }
    
    # Record average AOD value in the average tibble. Returns NA only if *all*
    # values at that location are NA.
    avgTb$AOD[p] <- mean(pointValues, na.rm = TRUE)
    
  }
  
  # ----- Create spatial points -------------------------------------------------
  
  sp <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(avgTb, c(.data$lon, .data$lat)),
    data = dplyr::select(avgTb, -c(.data$lon, .data$lat))
  )
  
  # ----- Return ---------------------------------------------------------------
  
  return(sp)
  
}

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  
  loadSpatialData("NaturalEarthAdm1")
  
  bbox_oregon <- c(-125, -116, 42, 47)
  
  # Create points from scans covering a full hour
  sp <- goesaodc_createMultiScanSPDF(
    satID = "G17",
    datetime = "2020-09-08 12",
    endtime = "2020-09-08 13",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon
  )
  
  # Plot points
  goesaodc_plotScanSPDF(sp) +
    AirFirePlots::layer_states("OR")
  
}