#' @export
#'
#' @title Convert and validate bbox objects or strings into standard format
#'
#' @description Accepts a variety of bbox formats, validates that they are
#' numerically sane and returns a vector of floats in c(lonLo, lonLo, latLo, 
#' latHi) order. Input can be a vector of floats in c(lonLo, lonHi, latLo, 
#' latHi) order or the return value from \code{sp::bbox()} or 
#' \code{raster::extent()}.
#'
#' @param bbox Bounding box for the region of interest.
#'
#' @return a vector of floats in c(lonLo, lonHi, latLo, latHi) order.
#'
#' @examples
#' \donttest{
#' library(MazamaSpatialUtils)
#' library(MazamaSatelliteUtils)
#' 
#' setSpatialDataDir("~/Data/Spatial")
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' loadSpatialData("USCensusStates")
#' 
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' bbox <- sp::bbox(oregon)
#' 
#' scanFile <- goesaodc_downloadScanFiles(
#'   satID = "G16", 
#'   datetime = "2019-09-06 18:00", 
#'   timezone = "UTC"
#' )
#' 
#' scanRaster <- goesaodc_createScanRaster(
#'   filename = scanFile,
#'   cellSize = 0.1,
#'   dqfLevel = 2
#' )
#' 
#' extent <- raster::extent(scanRaster)
#' 
#' # Convert bbox
#' bbox
#' bboxToVector(bbox)
#' 
#' # Convert extent
#' extent
#' bboxToVector(extent)
#' 
#' # Convert low/hi mixup
#' bboxToVector(c(-116, -124, 42, 46))
#' }
#' 

bboxToVector <- function(
  bbox = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(bbox)
  
  # ----- Extract data ---------------------------------------------------------
  
  if (
    "matrix" %in% class(bbox) &&
    all(colnames(bbox) == c("min", "max")) &&
    all(rownames(bbox) == c("x", "y"))
  ) {
    
    # Support for sp::bbox() type
    w <- bbox[1, 1]
    e <- bbox[1, 2]
    s <- bbox[2, 1]
    n <- bbox[2, 2]
    
  } else if ( all(class(bbox) == "Extent" && typeof(bbox) == "S4") ) {
    
    # Support for raster::extent() type
    w <- bbox[1]
    e <- bbox[2]
    s <- bbox[3]
    n <- bbox[4]
    
  } else if ( all(is.numeric(bbox) && is.vector(bbox)) ) {
    
    if ( length(bbox) != 4 ) 
      stop("Parameter 'bbox' should be of length 4. Try c(W, E, S, N).")
    
    # Support for vector of floats type
    w <- bbox[1]
    e <- bbox[2]
    s <- bbox[3]
    n <- bbox[4]
    
  } else {
    
    stop("Parameter 'bbox' is not recognized. Try c(W, E, S, N).")
    
  }

  # ----- Check order and extent -----------------------------------------------
  
  lonLo <- min(w, e)
  lonHi <- max(w, e)
  latLo <- min(s, n)
  latHi <- max(s, n)
  
  if ( lonLo < -180 || lonLo > 180 ||
       lonHi < -180 || lonHi > 180 )
    stop("Longitude must be defined in -180:180")
  
  if ( latLo < -90 || latLo > 90 ||
       latHi < -90 || latHi > 90 )
    stop("Latitude must be defined in -90:90")
  
  # ----- Return ---------------------------------------------------------------
  
  return( c(lonLo, lonHi, latLo, latHi) )
  
}
