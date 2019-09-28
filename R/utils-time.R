#' @export
#' 
#' @title check if specified \code{datetime} is during the daylight hours of the
#' specified \code{region}
#' 
#' @param datetime desired datetime in any Y-m-d H [MS] format or \code{POSIXct}
#' @param bbox bounding box for region of interest
#' @param timezone timezone in which to interpret the \code{datetime}
#' 
# ROGER:  Flesh out documentation
#' 
#' @return logical

isDaylight <- function(
  datetime = NULL,
  bbox = NULL,
  timezone = "UTC"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(datetime)
  MazamaCoreUtils::stopIfNull(bbox)
  
  datetime <- MazamaCoreUtils::parseDatetime(datetime, timezone)
  
  # ----- Extract boundaries ---------------------------------------------------
  
  if ( 
    class(bbox) == "matrix" &&
    all(colnames(bbox) == c("min","max")) &&
    all(rownames(bbox) == c("x","y"))
  ) {
    # > bbox(us)
    # min       max
    # x -117.12238 -86.77278
    # y   14.55055  32.71846
    w <- bbox[1,1]
    e <- bbox[1,2]
    s <- bbox[2,1]
    n <- bbox[2,2]
  } else {
    # ROGER:  support raster::extent() type bbox
    stop("bbox type not recognized")
  }
  
  mid_lat <- s + (n-s)/2
  
  # ----- Calculate sunrise and sunset -----------------------------------------
  
  # maptools::sunriset() requires a matrix
  west_edge <- matrix(c(w, mid_lat), nrow = 1)
  east_edge <- matrix(c(e, mid_lat), nrow = 1)
  
  # sunrise for mid_lat on East edge
  sunriseDF <- maptools::sunriset(east_edge, 
                                  datetime, 
                                  direction = "sunrise",
                                  POSIXct.out = TRUE)
  
  # sunset for mid_lat West edge
  sunsetDF <- maptools::sunriset(west_edge, 
                                 datetime, 
                                 direction = "sunset",
                                 POSIXct.out = TRUE)
  
  # Extract vectors
  sunrise <- sunriseDF[,2] 
  sunset <- sunsetDF[,2]
  
  # ----- check if datetime is during daylight hours ---------------------------
  
  if ( datetime > sunrise && datetime < sunset ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}
