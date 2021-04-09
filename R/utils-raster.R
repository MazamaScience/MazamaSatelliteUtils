#' @keywords utils
#' 
#' @export
#' 
#' @title Get raster value at a point
#' 
#' @description Gets the raster value at a given point.
#' 
#' @param raster A \code{RasterLayer} object.
#' @param x Longitude value (deg E).
#' @param y Latitude value (deg N).
#' 
#' @return Value of raster cell covering the point.

getValue <- function(
  raster,
  x,
  y
) {
  
  value <- raster::getValues(raster)[raster::cellFromXY(raster, c(x, y))]
  return(value)
  
}
