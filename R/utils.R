#' @export
#'
#' @title Converts and validates bbox objects or strings into standard format
#'
#' @param bbox bounding box for region of interest.
#'
#' @description Accepts a variety of bbox formats, validates that they are
#' numerically sane and returns a vector of floats in c(lonLo, lonLo, latLo, latHi) order.
#' Input can be of type \code{sp::bbox}, \code{raster::extent} or a vector of
#' floats in c(lonLo, lonHi, latLo, latHi) order.
#'
#' @return a vector of floats in c(lonLo, lonHi, latLo, latHi) order.
#'
#' @examples
#' \donttest{
#' # USE SAMPLE CODE FROM isDaylight.R
#' }
#' 

bboxToVector <- function(
  bbox = NULL
) {
  
  if (
    class(bbox) == "matrix" &&
    all(colnames(bbox) == c("min", "max")) &&
    all(rownames(bbox) == c("x", "y"))
  ) {
    
    # Support for sp::bbox() type
    w <- bbox[1, 1]
    e <- bbox[1, 2]
    s <- bbox[2, 1]
    n <- bbox[2, 2]
    
    # Trust order and domain
    
  } else if (
    class(bbox) == "Extent" &&
    typeof(bbox) == "S4"
  ) {
    
    # Support for raster::extent() type
    w <- bbox[1]
    e <- bbox[2]
    s <- bbox[3]
    n <- bbox[4]
    
    # Trust order and domain
    
  } else if (
    is.numeric(bbox) &&
    is.vector(bbox)
  ) {
    
    # Support for vector of floats in c(lonLo, lonHi, latLo, latHi)
    w <- min(bbox[1], bbox[2])
    e <- max(bbox[1], bbox[2])
    s <- min(bbox[3], bbox[4])
    n <- max(bbox[3], bbox[4])
    
    if (w <= -180 || w >= 180 ||
        e <= -180 || e >= 180)
      stop("Longitude must be defined in -180:180")
    
    if (s <= -90 || s >= 90 ||
        n <= -90 || n >= 90)
      stop("Latitude must be defined in -90:90")
    
  } else {
    
    stop("Parameter 'bbox' is not recognized. Try c(W, E, S, N).")
    
  }

    return( c(w, e, s, n) )
  
}
