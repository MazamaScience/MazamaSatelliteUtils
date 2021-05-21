#' @export
#' 
#' @title Create a raster from a GOES AOD scan
#' 
#' @description Creates a \code{RasterLayer} of AOD readings from a GOES scan, 
#' file or a \code{RasterBrick} of \code{RasterLayer}s from a series of files.
#' 
#' @param filename Name of a scan file.
#' @param bbox Bounding box for the region of interest. All points outside of 
#' this area will be removed and therefore won't be rasterized. Defaults to 
#' CONUS.
#' @param dqfLevel Data quality flag level. Defaults to 3.
#' @param cellSize Size of Raster cells measured in degrees lon/lat.
#' @param fun Function used to summarize point values within a single cell. 
#' Defaults to \code{mean}.
#' @param na.rm Logical flag determining whether to remove \code{NA} values 
#' before summarizing them with `fun`. Defaults to \code{FALSE}.
#' 
#' @return \code{RasterLayer} or \code{RasterBrick} of AOD values.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bboxOregon <- c(-125, -116, 42, 46.5)
#' 
#' scanFiles <- goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' # Create a raster for a scan file
#' goesaodc_createScanRaster(
#'   filename = scanFiles[1],
#'   bbox = bboxOregon,
#'   dqfLevel = 2,
#'   cellSize = 0.05
#' )
#' 
#' # Create a raster from scans averaged over a time range
#' goesaodc_createScanRaster(
#'   filename = scanFiles,
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
  filename = NULL,
  bbox = bbox_CONUS,
  dqfLevel = 3,
  cellSize = NULL,
  fun = mean,
  na.rm = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(filename)
  MazamaCoreUtils::stopIfNull(cellSize)
  
  if ( !(dqfLevel %in% c(0, 1, 2, 3)) )
    stop(paste0("Parameter 'dqfLevel' must be 0, 1, 2, or 3"))
  
  # ----- Define raster grid ---------------------------------------------------
  
  filePattern <- "OR_ABI-L2-AODC-M[0-9]_(G16|G17)_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  satID <- stringr::str_match(filename, filePattern)[1,2]
  
  # Get the grid file for the requested satellite
  if ( toupper(satID) == "G16" ) {
    gridFile <- "goesEastGrid.rda"
  } else if ( toupper(satID) == "G17" ) {
    gridFile <- "goesWestGrid.rda"
  } else {
    stop("Scan file satellite ID does not match 'G16' or 'G17'")
  }
  
  # Assemble the correct filepath based on satID and Data directory
  gridFilePath <- file.path(getSatelliteDataDir(), gridFile)
  
  # Test for grid existence and if found, load it. Stop with appropriate message
  # if missing
  if ( file.exists(gridFilePath) ) {
    goesGrid <- get(load(gridFilePath))
  } else {
    stop(paste0("Grid file '", gridFilePath, "'not found. Run 
    'goesaodc_installGoesGrids()' first."))
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
  spdf <- goesaodc_createScanSpdf(
    filename = filename,
    bbox = bbox,
    dqfLevel = dqfLevel
  )
  
  if ( "list" %in% class(spdf) ) {
    
    spdfList <- spdf
    rasterList <- list()
    
    for ( i in seq_along(spdfList) ) {
      scanLabel <- names(spdfList)[i]
      raster <- raster::rasterize(
        spdfList[[i]],
        scanRaster,
        field = "AOD",
        fun = fun,
        na.rm = na.rm
      )
      rasterList[[scanLabel]] <- raster
    }
    
    rasterBrick <- raster::brick(rasterList)
    
    layerNames <- paste0("t_",
      filename %>%
      goesaodc_convertFilenameToDatetime() %>%
      MazamaCoreUtils::timeStamp(timezone = "UTC")
    )

    names(rasterBrick) <- layerNames
    
    return(rasterBrick)
    
  } else {
    
    raster <- raster::rasterize(
      spdf,
      scanRaster,
      field = "AOD",
      fun = fun,
      na.rm = na.rm
    )
    
    names(raster) <- "AOD"
    
    return(raster)
    
  }
  
}
