#' @export
#' 
#' @title Create rasters from GOES scans
#' 
#' @description Creates a \code{RasterBrick} of AOD and DQF readings from a GOES
#' scan, or a list of \code{RasterBrick}s from a series of GOES scans.
#' 
#' @details A single scan can be identified by either giving its 
#' \code{filename}, or by providing the \code{satID}, \code{datetime}, and 
#' \code{timezone} (which will be used to determine the closest matching scan). 
#' A series of scans cannot be specified using filenames. The satellite + time 
#' info must be given along with an \code{endtime} so that all scans starting at
#' \code{datetime} up to (but not including) \code{endtime} will be processed.
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
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' library(MazamaSpatialUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' setSpatialDataDir("~/Data/Spatial")
#' 
#' loadSpatialData("NaturalEarthAdm1")
#' 
#' bboxOregon <- c(-125, -116, 42, 46.5)
#' 
#' # Create a raster for a scan specified by satellite + time
#' goesaodc_createScanRaster(
#'   satID = "G17",
#'   datetime = "2020-09-08 17:30",
#'   timezone = "America/Los_Angeles",
#'   bbox = bboxOregon,
#'   cellSize = 0.05
#' )
#' 
#' # Create a raster for a scan file
#' goesaodc_createScanRaster(
#'   filename = "OR_ABI-L2-AODC-M6_G17_s20202530031174_e20202530033547_c20202530035523.nc",
#'   bbox = bboxOregon,
#'   dqfLevel = 2,
#'   cellSize = 0.05
#' )
#' 
#' # Create a raster from scans averaged over a time range
#' goesaodc_createScanRaster(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles",
#'   bbox = bboxOregon,
#'   cellSize = 0.05
#' )
#' 
#' # Create a raster for a faulty scan
#' goesaodc_createScanRaster(
#'   filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
#'   bbox = bboxOregon,
#'   cellSize = 0.05
#' )
#' }

goesaodc_createScanRaster <- function(
  satID = NULL,
  datetime = NULL,
  endtime = NULL,
  timezone = "UTC",
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3,
  cellSize = NULL,
  fun = mean
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(cellSize)
  
  # ----- Define raster grid ---------------------------------------------------
  
  # If a filename was given, extract the satellite ID from it
  if ( is.null(satID) ) {
    MazamaCoreUtils::stopIfNull(filename)
    filePattern <- "OR_ABI-L2-AODC-M[0-9]_(G16|G17)_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
    satID <- stringr::str_match(filename, filePattern)[1,2]
  }
  
  # Get the grid file for the requested satellite
  if ( toupper(satID) == "G16" ) {
    gridFile <- "goesEastGrid.rda"
  } else if ( toupper(satID) == "G17" ) {
    gridFile <- "goesWestGrid.rda"
  } else {
    stop("Parameter 'satID' must be either 'G16' or 'G17'")
  }
  
  # Assemble the correct filepath based on satID and Data directory
  gridFilePath <- file.path(getSatelliteDataDir(), gridFile)
  
  # Test for grid existence and if found, load it. Stop with appropriate message
  # if missing
  if ( file.exists(gridFilePath) ) {
    goesGrid <- get(load(gridFilePath))
  } else {
    stop(paste0("Grid file '", gridFilePath, "'not found. Run 
    'installGoesGrids()' first."))
  }
  
  # Determine grid extent
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
  
  scanRaster <- raster::raster(
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
  spdf <- goesaodc_createScanPoints(
    satID = satID,
    datetime = datetime,
    endtime = endtime,
    timezone = timezone,
    filename = filename,
    bbox = bbox,
    dqfLevel = dqfLevel
  )
  
  if ( "list" %in% class(spdf) ) {
    
    spdfList <- spdf
    rasterList <- list()
    
    for ( i in 1:length(spdfList) ) {
      scanLabel <- names(spdfList)[i]
      raster <- raster::rasterize(
        spdfList[[i]],
        scanRaster,
        field = "AOD",
        fun = fun
      )
      rasterList[[scanLabel]] <- raster
    }
    
    return(rasterList)
    
  } else {
    
    raster <- raster::rasterize(
      spdf,
      scanRaster,
      field = "AOD",
      fun = fun
    )
    
    return(raster)
    
  }
  
}
