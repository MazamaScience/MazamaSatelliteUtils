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
#' # USE SAMPLE CODE FROM is_daylight.R
#' }
#' 

bboxToVector <- function(
  bbox = NULL
) {
  # SUPPORT FOR sp::bbox TYPE
  if (
    class(bbox) == "matrix" &&
    all(colnames(bbox) == c("min", "max")) &&
    all(rownames(bbox) == c("x", "y"))
  ) {
    w <- bbox[1, 1]
    e <- bbox[1, 2]
    s <- bbox[2, 1]
    n <- bbox[2, 2]
  # SUPPORT FOR raster::extent TYPE
  } else if (
    class(bbox) == "Extent" &&
    typeof(bbox) == "S4"
  ) {
    w <- bbox[1]
    e <- bbox[2]
    s <- bbox[3]
    n <- bbox[4]
  # SUPPPORT FOR VECTOR OF FLOATS IN c(lonLo, lonHi, latLo, latHi) ORDER
  } else if (
    is.numeric(bbox) &&
    is.vector(bbox)
  ) {
    w <- bbox[1]
    e <- bbox[2]
    s <- bbox[3]
    n <- bbox[4]
  } else {
    stop("bbox type not recognized")
  }
  lonLo <- w
  lonHi <- e
  latLo <- s
  latHi <- n
  # ADD SOME SANITY CHECKING HERE -180 <= lon <= 180, -90 <= lat <= 90
  # Lo COORDS SHOULD BE LESS THAN Hi COORDS
  return( c(lonLo, lonHi, latLo, latHi) )
}
