#' @export
#' 
#' @title Create Rasters from GOES scans
#' 
#' @description Creates a Raster of AOD readings from one or more GOES scans. 
#' Scans are specified either by filename or by satellite + time information.
#' 
#' @details
#' If \code{filename} is given, just data for that scan file will be 
#' processed. Otherwise, scan(s) must be specified by \code{satID}, 
#' \code{datetime}, and \code{timezone}, which will determine the closest scan 
#' taken to that time. An average Raster of a series of scans can be requested 
#' by also supplying \code{endtime}. All scans taken from \code{datetime} 
#' (inclusive) up to \code{endtime} (exclusive) will be processed by taking the 
#' average value of each reading point and rasterizing the result (not by 
#' rasterizing each scan and then taking the average).
#' 
#' @param satID ID of the source GOES satellite ('G16' or 'G17').
#' @param datetime Datetime in Ymd HMS format or a \code{POSIXct}.
#' @param endtime End time in Ymd HMS format or a \code{POSIXct}.
#' @param timezone Timezone used to interpret \code{datetime} and 
#' \code{endtime}; Defaults to UTC.
#' @param filename Name of a scan file.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level; Defaults to NULL.
#' @param cellSize Size of Raster cells measured in degrees lon/lat.
#' @param fun Function used to summarize multiple point values within a single 
#' cell; Defaults to \code{mean}.

goesaodc_createScanRaster <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = NULL,
  cellSize = NULL,
  fun = mean
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(cellSize)
  
  # ----- Define raster grid ---------------------------------------------------
  
  # Set extent
  if ( !is.null(bbox) ) {
    boundaries <- bboxToVector(bbox)
    lonLo <- boundaries[1]
    lonHi <- boundaries[2]
    latLo <- boundaries[3]
    latHi <- boundaries[4]
  } else {
    lonLo <- min(goesGrid$longitude, na.rm = TRUE)
    lonHi <- max(goesGrid$longitude, na.rm = TRUE)
    latLo <- min(goesGrid$latitude, na.rm = TRUE)
    latHi <- max(goesGrid$latitude, na.rm = TRUE)
  }
  
  ncols <- ( ( lonHi - lonLo ) / cellSize ) + 1
  nrows <- ( ( latHi - latLo ) / cellSize ) + 1
  
  raster <- raster::raster(
    nrows = nrows, 
    ncols = ncols,
    xmn = lonLo, 
    xmx = lonHi,
    ymn = latLo, 
    ymx = latHi,
    res = cellSize,
    crs = "+proj=longlat +datum=WGS84 +ellps=GRS80"
  )
  
  # ----- Create raster --------------------------------------------------------
  
  # Get spatial points data
  spdf <- goesaodc_createScanSPDF(
    satID = satID,
    datetime = datetime,
    endtime = endtime,
    timezone = timezone,
    filename = filename,
    bbox = bbox,
    dqfLevel = dqfLevel
  )
  
  # Rasterize spatial points
  raster <- raster::rasterize(spdf, raster, fun = fun)
  
  # ----- Return ---------------------------------------------------------------
  
  return(raster)
  
}

if ( FALSE ) {
  
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
  
  setSatelliteDataDir("~/Data/Satellite")
  setSpatialDataDir("~/Data/Spatial")
  
  loadSpatialData("NaturalEarthAdm1")
  
  bbox_oregon <- c(-125, -116, 42, 46.5)
  
  # Create points from a scan specified by satellite + time
  goesaodc_createScanRaster(
    satID = "G17",
    datetime = "2020-09-08 17:30",
    timezone = "America/Los_Angeles",
    bbox = bbox_oregon,
    cellSize = 0.2
  )
  
  # Create points from a named scan file
  oregonRaster <- goesaodc_createScanRaster(
    filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
    bbox = bbox_oregon,
    cellSize = 0.04
  )
  
  coords <- data.frame(lon = -124, lat = 44)

  raster::extract(
    x = oregonRaster,
    y = coords, 
    method = "simple",
    buffer = 1,
    fun = mean
  )[,"AOD"]
  
  AirFirePlots::plot_raster(
    oregonRaster,
    varName = "AOD",
    breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    palette = "YlOrRd"
  ) +
    AirFirePlots::layer_states("OR")
  
}