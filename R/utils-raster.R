#' @keywords utils
#' @export
#' @title Get raster value at a point
#' @param raster \code{RasterLayer} object
#' @param x longitude value (deg E)
#' @param y latitude value (deg N)
#' @description Get raster value at a point.
#' @return Value of raster cell covering the point.

getValue <- function(raster, x, y) {
  value <- raster::getValues(raster)[raster::cellFromXY(raster, c(x, y))]
  return(value)
}


 
