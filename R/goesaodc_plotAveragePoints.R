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
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' oregon_bbox <- c(-125, -116, 42, 47)
#' 
#' goesaodc_plotAveragePoints(
#'   satID = "G17",
#'   datetime = "2020-09-8 18",
#'   timezone = "America/Los_Angeles",
#'   bbox = oregon_bbox,
#'   cex = 0.3
#' )
#' 
#' maps::map(
#'   database = "state",
#'   regions = "oregon",
#'   xlim = oregon_bbox[1:2],
#'   ylim = oregon_bbox[3:4],
#'   add  = TRUE
#' )
#' 
#' 
#' kincade_bbox <- c(-124, -120, 36, 39)
#' 
#' goesaodc_plotAveragePoints(
#'   satID = "G17",
#'   datetime = "2019-10-27 10",
#'   endtime = "2019-10-27 14",
#'   timezone = "America/Los_Angeles",
#'   bbox = kincade_bbox,
#'   cex = 0.5
#' )
#' 
#' maps::map(
#'   database = "state",
#'   regions = "california",
#'   xlim = kincade_bbox[1:2],
#'   ylim = kincade_bbox[3:4],
#'   add  = TRUE
#' )
#' }

goesaodc_plotAveragePoints <- function(
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
    
    # Record average AOD value in the average tibble. Returns NA only if *all*
    # the point's values are NA.
    avgTb$AOD[p] <- mean(pointValues, na.rm = TRUE)
    
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
