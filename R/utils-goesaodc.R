# Code refactored from Sean Raffuse's goesfire repository:
#   https://github.com/raffscallion/goesfire

#' @export
#' @importFrom ncdf4 ncvar_get
#'
#' @title Determines the x and y bounds (in radians) for a given netcdf handle
#'
#' @param nc netcdf handle
#'
#' @return A named list with x1, x2, y1, and y2.
#'
goesaodc_getCoordBounds <- function(nc) {
  
  x_bounds <- ncvar_get(nc, "x_image_bounds")
  y_bounds <- ncvar_get(nc, "y_image_bounds")
  
  bounds <- list(x1 = x_bounds[1], 
                 x2 = x_bounds[2], 
                 y1 = y_bounds[1], 
                 y2 = y_bounds[2])
  
  return(bounds)
  
}


#' @export
#' @importFrom ncdf4 ncatt_get
#'
#' @title Creates a data frame of decimal degree lon/lat pairs for a given
#' netcdf handle
#'
#' @param nc netcdf handle
#'
#' @description Code borrowed from https://github.com/raffscallion/goesfire/blob/master/R/utils.R
#'
#' @return Dataframe.
#'  
goesaodc_getCoordGrid <- function(nc) {
  
  # Get the x and y variables
  x <- ncvar_get(nc, varid = "x")
  y <- ncvar_get(nc, varid = "y")
  
  # Convert to radians
  x_scale <- ncatt_get(nc, "x", "scale_factor")$value
  y_scale <- ncatt_get(nc, "y", "scale_factor")$value
  x_offset <- ncatt_get(nc, "x", "add_offset")$value
  y_offset <- ncatt_get(nc, "y", "add_offset")$value
  
  # Create an initial tibble with "x", "y", "x_rad" and "y_rad"
  x_array <- replicate(length(y), x)
  y_array <- t(replicate(length(x), y))
  df <- dplyr::tibble(x = as.vector(x_array),
                      y = as.vector(y_array)) %>%
    dplyr::mutate(x_rad = x * x_scale + x_offset,
                  y_rad = y * y_scale + y_offset)
  
  # Get the parameters needed for geolocation
  r_eq <- ncatt_get(nc, "goesaodc_imager_projection", "semi_major_axis")$value
  r_pol <- ncatt_get(nc, "goesaodc_imager_projection", "semi_minor_axis")$value
  perspective_point <- ncatt_get(nc, "goesaodc_imager_projection",
                                 "perspective_point_height")$value
  H <- perspective_point + r_eq
  lambda0 <- ncatt_get(nc, "goesaodc_imager_projection",
                       "longitude_of_projection_origin")$value * (pi / 180)
  
  # ----- Convert scan angles to latitude and longitude ------------------------
  
  # @export
  #
  # @title Converts GOES xy radian coordinates to latitude and longitude pairs
  #
  # @param x x value from the netcdf in radians (scale and offset already applied)
  # @param y y value from the netcdf in radians (scale and offset already applied)
  # @param r_eq semi_major_axis
  # @param r_pol semi_minor_axis
  # @param H satellite height from center of earth = perspective_point_height +
  #   semi_major_axis
  # @param lambda0 longitude_of_projection_origin (converted to radians)
  #
  # @return a named list with lon and lat values
  #
  
  goesaodc_lonLat <- function(x, y, r_eq, r_pol, H, lambda0) {
    
    # Calculate distnace from satellite to point of interest
    a <- sin(x)^2 + cos(x)^2 * (cos(y)^2 + (r_eq^2 / r_pol^2) * sin(y)^2)
    b <- -2 * H * cos(x) * cos(y)
    c <- H^2 - r_eq^2
    r_s <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
    
    # Satellite coordinates
    sx <- r_s * cos(x) * cos(y)
    sy <- -r_s * sin(x)
    sz <- r_s * cos(x) * sin(y)
    
    # Coordinates in degrees
    lon <- ((lambda0 - atan(sy / (H - sx))) * 180) / pi
    lat <- (atan((r_eq^2 / r_pol^2) * (sz / sqrt((H - sx)^2 + sy^2))) * 180) / pi
    
    return(list("lon" = lon, "lat" = lat))
    
  }
  
  # geolocate
  df <- dplyr::bind_cols(df, 
                         purrr::map2_dfr(df$x_rad, df$y_rad,        # variables
                                         goesaodc_lonLat,               # function
                                         r_eq, r_pol, H, lambda0))  # constants
  
  return(df)
  
}


#' @export
#' 
#' @title Get projection metadata from GOES-R ABI ncdf4 object
#' @param nc ncdf4 object
#' @description Return the projection information of `nc`
#' @return list of metadata
#' 
goesaodc_getProjection <- function(
  nc
) {
  projection <- ncdf4::ncatt_get(nc, "goes_imager_projection")
  return(projection)
}

#' @export
#' 
#' @title Check if `nc` projection is GOES projection
#' @param nc ncdf4 object
#' @description 
#' Return TRUE if projection information in `nc` matches GOES projection
#' information.
#' 
#' @return logical

goesaodc_isGoesProjection <- function(
  nc
) {
  projection <- goesaodc_getProjection(nc)
  return(all(unlist(projection) == unlist(MazamaSatelliteUtils::goesEastGrid$projection)))
}

#' @export
#' 
#' @title Create a quick plot of a GOES AOD SpatialPointsDataFrame
#' @param spatialPoints SpatialPointsDataFrame
#' @param var Variable to plot
#' @param n Sample size
#' @param breaks vector of color breaks
#' @param pch plot character
#' @param cex plot symbol scale factor
#' @param paletteName RColorBrewer palette name
#' @param add logical to add points to current plot
#' 
#' @description Quickly subsample and plot points in a GOES AOD 
#' spatialPointsDataFrame

goesaodc_plotSpatialPoints <- function(
  spatialPoints,
  var = "AOD",
  n = 1e5,
  breaks = NULL,
  pch = 15,
  cex = 0.5,
  paletteName = "YlOrRd",
  add = FALSE
) {
  
  # Subsample points
  indices <- sample(seq_len(nrow(spatialPoints)), n)
  spatialPointsSub <- spatialPoints[indices,]
  
  # Make breaks for specifed number of equally sized color bins
  # TODO: Use quantiles
  if (is.null(breaks)) {
    breaks <- quantile(spatialPointsSub[[var]])
  }
  
  cols <- RColorBrewer::brewer.pal(length(breaks)-1, paletteName)
  col_i <- .bincode(spatialPointsSub[[var]], breaks)
  col_v <- cols[col_i]
  plot(spatialPointsSub, pch=pch, col=col_v, cex=cex, add=add)
}


#' @export
#' 
#' @title get the scan start time from a GOES AOD netCDF file name
#' @param file GOES AOD netCDF file name
#' 
#' @return scan start time 

goesaodc_getStartTime <- function(file) {
  stringr::str_split(file, "_") %>% 
    unlist() %>% 
    dplyr::nth(-3) %>% 
    stringr::str_sub(2, -1)
}

