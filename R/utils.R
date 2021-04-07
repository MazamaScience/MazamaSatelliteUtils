#' @export
#'
#' @title Converts and validates bbox objects or strings into standard format
#'
#' @param bbox bounding box for region of interest.
#'
#' @description Accepts a variety of bbox formats, validates that they are
#' numerically sane and returns a vector of floats in c(lonLo, lonLo, latLo, latHi) order.
#' Input can be a vector of floats in c(lonLo, lonHi, latLo, latHi) order or 
#' the return value from \code{sp::bbox()} or \code{raster::extent()}.
#'
#' @return a vector of floats in c(lonLo, lonHi, latLo, latHi) order.
#'
#' @examples
#' \donttest{
#' library(MazamaSpatialUtils)
#' library(MazamaSatelliteUtils)
#' 
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusStates")
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' oregon <- subset(USCensusStates, stateCode == "OR")
#' bbox <- sp::bbox(oregon)
#' 
#' goesaodc_downloadAOD(
#'   satID = "G16", 
#'   datetime = "201924918", 
#'   timezone = "UTC", 
#'   isJulian = TRUE)
#'   
#' ncFile <- "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc"
#' nc <- goesaodc_openFile(ncFile)
#' 
#' raster <- goesaodc_createRaster(nc, res = 0.1, dqfLevel = 2)
#' extent <- raster::extent(raster)
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
    all(class(bbox) == c("matrix", "array")) &&
    all(colnames(bbox) == c("min", "max")) &&
    all(rownames(bbox) == c("x", "y"))
  ) {
    
    # Support for sp::bbox() type
    w <- bbox[1, 1]
    e <- bbox[1, 2]
    s <- bbox[2, 1]
    n <- bbox[2, 2]
    
  } else if (
    class(bbox) == "Extent" &&
    typeof(bbox) == "S4"
  ) {
    
    # Support for raster::extent() type
    w <- bbox[1]
    e <- bbox[2]
    s <- bbox[3]
    n <- bbox[4]
    
  } else if (
    is.numeric(bbox) &&
    is.vector(bbox)
  ) {
    
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
