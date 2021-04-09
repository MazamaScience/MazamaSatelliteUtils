#' @export
#'
#' @title Create a RasterBrick of GOES data
#' 
#' @description Create a RasterBrick of GOES AOD data including data points
#' with the specified resolution and function, and within the specified bounding
#' box and data quality flag level.
#' 
#' The \code{bbox} parameter can be a vector of floats in c(lonLo, lonHi, latLo,
#' latHi) order or the return value from \code{sp::bbox()} or 
#' \code{raster::extent()}.
#' 
#' The \code{dqfLevel} parameter can take a value of:
#'
#' \itemize{
#' \item{0}{ -- High quality retrieval flag}
#' \item{1}{ -- Medium quality retrieval flag}
#' \item{2}{ -- Low quality retrieval flag}
#' \item{3}{ -- No retrieval quality flag}
#' }
#'
#' @param nc ncdf4 handle.
#' @param res Resolution of raster in degrees; Defaults to 0.1.
#' @param fun Function to use when rasterizing; Defaults to mean.
#' @param bbox Bounding box for the region of interest; Defaults to CONUS.
#' @param dqfLevel Data quality flag level.
#'
#' @return RasterBrick
#'
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#'
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' netCDF <- goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = 2019102714, 
#'   timezone = "America/Los_Angeles",
#'   verbose = TRUE 
#' )[1]
#' 
#' # Kincade fire region
#' kincade_bbox <- c(-124, -120, 36, 39)
#' 
#' nc <- goesaodc_openFile(netCDF)
#' raster <- goesaodc_createRaster(
#'   nc, 
#'   res = 0.03,
#'   bbox = kincade_bbox,
#'   dqfLevel = 3
#' )
#'   
#' west <- c("washington", "oregon", "california", "nevada", "idaho")
#' maps::map(database = "state", regions = west)   
#' raster::plot(raster, "AOD", col=rev(heat.colors(5)), add = TRUE)
#' maps::map(database = "state", regions = west, add = TRUE)
#' }

goesaodc_createRaster <- function(
  nc = NULL,
  res = 0.1,
  fun = mean,
  bbox = bbox_CONUS,
  dqfLevel = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(bbox)
  MazamaCoreUtils::stopIfNull(dqfLevel)
  
  # Check that nc has GOES projection
  if ( !goesaodc_isGoesProjection(nc) ) {
    stop("Parameter 'nc' does not have standard GOES-R projection information.")
  }
  
  # ----- Get grid data --------------------------------------------------------
  
  # Get satID from netCDF, will be either "G16" or "G17"
  satID <- ncdf4::ncatt_get(nc, varid = 0, attname = "platform_ID")$value
  
  # Choose which gridFile to load based on satID
  if ( toupper(satID) == "G16") {
    gridFile <- "goesEastGrid.rda"
  } else if ( toupper(satID) == "G17" ) {
    gridFile <- "goesWestGrid.rda"
  } else {
    stop("Parameter 'satID' must be either 'G16' or 'G17'")
  }
  
  # Assemble the correct filepath based on satID and Data directory
  filePath <- file.path(getSatelliteDataDir(), gridFile)
  
  # Check that grids are present
  if ( !file.exists(filePath) ) {
    stop(paste0(filePath, " not found. Run 'installGoesGrids()."))
  }
  
  # Test for grid existence and if found, load it. Stop with appropriate message
  # if missing
  if ( file.exists(filePath) ) {
    goesGrid <- get(load(filePath))
  } else {
    stop("Grid file not found. Run 'installGoesGrids()' first")
  }
  
  # ----- Create Raster --------------------------------------------------------
  
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
  
  ncols <- ( ( lonHi - lonLo ) / res ) + 1
  nrows <- ( ( latHi - latLo ) / res ) + 1
  
  raster <- raster::raster(
    nrows = nrows, 
    ncols = ncols,
    xmn = lonLo, 
    xmx = lonHi,
    ymn = latLo, 
    ymx = latHi,
    res = res,
    crs = "+proj=longlat +datum=WGS84 +ellps=GRS80"
  )
  
  # ----- Create SpatialPointsDataFrame ----------------------------------------
  
  spatialPoints <- goesaodc_createSpatialPoints(
    nc = nc,
    bbox = bbox,
    dqfLevel = dqfLevel
  )
  
  # ----- Create RasterBrick ---------------------------------------------------
  
  rasterBrick <- raster::rasterize(spatialPoints, raster, fun = fun)
  
  # ----- Return ---------------------------------------------------------------
  
  return(rasterBrick)
  
}
