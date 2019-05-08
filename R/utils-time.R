#' @export
#' 
#' @title check if specified \code{datetime} is during the daylight hours of the
#' specified \code{region}
#' 
#' @param datetime desired datetime in any Y-m-d H [MS] format or \code{POSIXct}
#' @param region a named region. Currently only \code{"CONUS"} is supported
#' 
#' @return logical


isDaylight <- function(
  datetime = NULL,
  region = "CONUS"
) {
  
  # ----- parse datetime -------------------------------------------------------
  
  if (!is.null(datetime)) {
    orders <- c("Ymd", "YmdH", "YmdHM", "YmdHMS")
    suppressWarnings({
      dt <- lubridate::parse_date_time(datetime, orders, tz = "UTC")
    })
    if (is.na(dt)) {
      stop("Parameter 'datetime' cannot be interpreted.")
    } 
  } else {
    stop("Parameter 'datetime' must be defined.")
  }
  
  # ----- get CONUS sunrise and sunset ------------------------
  
  if (region == "CONUS") {
    
    # East Coast and West Coast coordinates determined as follows:
    # 
    # > states_conus <- subset(USCensusStates, stateCode %in% PWFSLSmoke::CONUS)
    # > bb <- bbox(states_conus)
    # > lat_midpoint <- (bb["y", "max"] - bb["y", "min"])/2 + bb["y", "min"]
    # > west_coast <- c(bb["x", "min"], lat_midpoint)
    # > west_coast
    # [1] -124.76307   36.95373
    # > east_coast <- c(bb["x", "max"], lat_midpoint)
    # east_coast
    # [1] -66.94989  36.95373
    
    # maptools::sunriset() requires a matrix
    west_coast <- matrix(c(-124.76307, 36.95373), nrow = 1)
    east_coast <- matrix(c(-66.94989, 36.95373), nrow = 1)
    
    # sunrise for East Coast
    sunrise <- maptools::sunriset(east_coast, dt, direction = "sunrise",
                                  POSIXct.out = TRUE)
    
    # sunset for West Coast
    sunset <- maptools::sunriset(west_coast, dt, direction = "sunset",
                                 POSIXct.out = TRUE)
    
    sunrise <- sunrise[,2]; sunset <- sunset[,2]
  }
  
  # ----- check if datetime is during daylight hours ---------------------------
  
  if (dt > sunrise && dt < sunset) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
