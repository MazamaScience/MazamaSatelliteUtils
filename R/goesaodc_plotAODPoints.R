#' @export
#' 
#' @title Create a point plot of averaged GOES AOD values
#' 
#' @description Averages the AOD values of each GOES sample location over 
#' multiple scans
#' 
#' @param satID ID of the source GOES satellite (G16 or G17).
#' @param datetime Datetime in any Ymd H [MS] format or \code{POSIXct}.
#' @param endtime End time in any Ymd H [MS] format or \code{POSIXct} 
#' (exclusive).
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dropNa Logical flag whether to drop rows with NA values.
#' @param breaks Vector of color breaks.
#' @param cex Plot symbol size.
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' # Kincade fire region
#' kincade_bbox <- c(-124, -120, 36, 39)
#' 
#' goesaodc_plotAODPoints(
#'   satID = "G16",
#'   datetime = "2019-10-27 10",
#'   timezone = "America/Los_Angeles",
#'   bbox = kincade_bbox,
#'   breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
#'   cex = 0.6
#' )
#'
#' maps::map(
#'   database = "state",
#'   regions = "california",
#'   xlim = c(-125, -120),
#'   ylim = c(36, 39),
#'   add  = TRUE
#' )
#' }

goesaodc_plotAODPoints <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = NULL,
  bbox = bbox_CONUS,
  dropNa = TRUE,
  breaks = NULL,
  cex = 0.5
) {
  
  # ----- Download scan files --------------------------------------------------
  
  fileList <- goesaodc_downloadAOD(
    satID = satID,
    datetime = datetime,
    endtime = endtime,
    timezone = timezone
  )
  
  # ----- Create netCDF handles ------------------------------------------------
  
  ncList <- list()
  
  for ( file in fileList ) {
    
    label <- 
      file %>%
      goesaodc_convertFilenameToDatetime() %>%
      MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
    
    ncList[[label]] <- goesaodc_openFile(basename(file))
    
  }
  
  # ----- Create scan tibbles --------------------------------------------------
  
  tbList <- list()
  
  for ( nc in ncList ) {
    
    label <- 
      basename(nc$filename) %>%
      goesaodc_convertFilenameToDatetime() %>%
      MazamaCoreUtils::timeStamp(unit = "sec", timezone = "UTC")
    
    # Do not drop NA rows, since all scan tibbles must be the same size
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
    
    # Record average AOD value in the average tibble
    avgTb$AOD[p] <- mean(pointValues, na.rm = dropNa)
    
  }
  
  # ----- Create SpatialPoints -------------------------------------------------
  
  sp <- sp::SpatialPointsDataFrame(
    coords = dplyr::select(avgTb, c(.data$lon, .data$lat)),
    data = dplyr::select(avgTb, -c(.data$lon, .data$lat))
  )
  
  # ----- Plot SpatialPoints ---------------------------------------------------
  
  goesaodc_plotSpatialPoints(
    spatialPoints = sp,
    breaks = breaks,
    cex = cex
  )
  
}
