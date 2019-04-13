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
goes_getCoordBounds <- function(nc) {
  
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
#' @return Dataframe.
#' 
goes_getCoordGrid <- function(nc) {
  
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
  r_eq <- ncatt_get(nc, "goes_imager_projection", "semi_major_axis")$value
  r_pol <- ncatt_get(nc, "goes_imager_projection", "semi_minor_axis")$value
  perspective_point <- ncatt_get(nc, "goes_imager_projection",
                                 "perspective_point_height")$value
  H <- perspective_point + r_eq
  lambda0 <- ncatt_get(nc, "goes_imager_projection",
                       "longitude_of_projection_origin")$value * (pi / 180)
  
  # geolocate
  df <- dplyr::bind_cols(df, 
                         purrr::map2_dfr(df$x_rad, df$y_rad,        # variables
                                         goes_lonLat,               # function
                                         r_eq, r_pol, H, lambda0))  # constants
  
  return(df)
  
}


#' @export
#'
#' @title Converts GOES xy radian coordinates to latitude and longitude pairs
#'
#' @param x x value from the netcdf in radians (scale and offset already applied)
#' @param y y value from the netcdf in radians (scale and offset already applied)
#' @param r_eq semi_major_axis
#' @param r_pol semi_minor_axis
#' @param H satellite height from center of earth = perspective_point_height +
#'   semi_major_axis
#' @param lambda0 longitude_of_projection_origin (converted to radians)
#'
#' @return a named list with lon and lat values
#'
goes_lonLat <- function(x, y, r_eq, r_pol, H, lambda0) {
  
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

# ------------------------------------------------------------------------------


#' Pull information from GOES-R ABI netcdf files and return a csv with lat/lon info
#'
#' @param filename the full path name of the netcdf FDC file to process
#' @param vars The variables to include in the output among c("Temp", "Power", "Area",
#'   "DQF", "Mask"). All are included by default.
#' @param maskvals If "Mask" is selected in vars, these are the mask values to keep. If no
#'   mask values are selected, all pixels will be returned and the output will be very
#'   large.
#'
#' @return A data frame with latitude, longitude, time, and the variables requested.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @import ncdf4
#'
extract_aod <- function(
  filename, 
  vars = c("AOD"),
  maskvals = NULL
) {
  
  # open the file
  nc <- nc_open(filename)
  
  # grab the requested items and splice them together
  get_variable <- function(var, nc) {
    
    # NOTE - ncvar_get applies scaling factor itself
    var_array <- ncvar_get(nc, var)
    #locs <- which(!is.na(var_array), arr.ind = TRUE)
    vals <- var_array[!is.na(var_array)]
    
    x <- ncvar_get(nc, "x")
    y <- ncvar_get(nc, "y")
    x_array <- replicate(length(y), x)
    y_array <- t(replicate(length(x), y))
    dplyr::tibble(x = x_array[!is.na(var_array)],
                  y = y_array[!is.na(var_array)],
                  Value = vals,
                  Parameter = var)
  }
  
  df <- purrr::map_dfr(vars, get_variable, nc) %>%
    tidyr::spread(.data$Parameter, .data$Value)
  
  if ("Mask" %in% vars & !is.null(maskvals)) {
    df <- dplyr::filter(df, .data$Mask %in% maskvals)
  }
  
  # If a variable is all null, it will need to be added here
  missing <- setdiff(vars,names(df))
  for (m in missing) {
    if (nrow(df) > 0) {
      df[[m]] <- NA
    } else {
      df[[m]] <- numeric(0)
    }
  }
  
  # If the requested data are empty, return an empty data frame with the correct shape
  if (nrow(df) == 0) {
    df <- dplyr::mutate(df, lon = numeric(0),
                        lat = numeric(0),
                        Filename = character(0),
                        StartTime = .POSIXct(double(0)),
                        EndTime = .POSIXct(double(0))) %>%
      dplyr::select(-.data$x , -.data$y)
    return(df)
  }
  
  # Get the parameters needed for geolocation
  x_scale <- ncatt_get(nc, "x", "scale_factor")$value
  y_scale <- ncatt_get(nc, "y", "scale_factor")$value
  x_offset <- ncatt_get(nc, "x", "add_offset")$value
  y_offset <- ncatt_get(nc, "y", "add_offset")$value
  r_eq <- ncatt_get(nc, "goes_imager_projection", "semi_major_axis")$value
  r_pol <- ncatt_get(nc, "goes_imager_projection", "semi_minor_axis")$value
  perspective_point <- ncatt_get(nc, "goes_imager_projection",
                                 "perspective_point_height")$value
  H <- perspective_point + r_eq
  lambda0 <- ncatt_get(nc, "goes_imager_projection",
                       "longitude_of_projection_origin")$value * (pi / 180)
  
  # geolocate
  df <- dplyr::mutate(df, x_rad = .data$x * x_scale + x_offset,
                      y_rad = .data$y * y_scale + y_offset) %>%
    dplyr::bind_cols(purrr::map2_dfr(.$x_rad, .$y_rad, goes_lonLat, r_eq,
                                     r_pol, H, lambda0))
  
}

#' Pull information from GOES-R ABI netcdf files and return a csv with lat/lon info
#' 
#' This version returns the full grid inlucding bad lon and lat values
#'
#' @param filename the full path name of the netcdf FDC file to process
#' @param vars The variables to include in the output among c("Temp", "Power", "Area",
#'   "DQF", "Mask"). All are included by default.
#' @param maskvals If "Mask" is selected in vars, these are the mask values to keep. If no
#'   mask values are selected, all pixels will be returned and the output will be very
#'   large.
#'
#' @return A data frame with latitude, longitude, time, and the variables requested.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @import ncdf4
#'
extract_full_aod <- function(
  filename, 
  vars = c("AOD"),
  maskvals = NULL
) {
  
  # open the file
  nc <- nc_open(filename)
  
  # Get AOD
  aod <- ncvar_get(nc, "AOD") #
  
  # grab the requested items and splice them together
  get_variable <- function(var, nc) {
    
    # NOTE - ncvar_get applies scaling factor itself
    var_array <- ncvar_get(nc, var)
    vals <- var_array
    
    x <- ncvar_get(nc, "x")
    y <- ncvar_get(nc, "y")
    x_array <- replicate(length(y), x)
    y_array <- t(replicate(length(x), y))
    dplyr::tibble(x = x_array,
                  y = y_array,
                  Value = vals,
                  Parameter = var)
  }
  
  df <- purrr::map_dfr(vars, get_variable, nc) %>%
    tidyr::spread(.data$Parameter, .data$Value)
  
  if ("Mask" %in% vars & !is.null(maskvals)) {
    df <- dplyr::filter(df, .data$Mask %in% maskvals)
  }
  
  # If a variable is all null, it will need to be added here
  missing <- setdiff(vars,names(df))
  for (m in missing) {
    if (nrow(df) > 0) {
      df[[m]] <- NA
    } else {
      df[[m]] <- numeric(0)
    }
  }
  
  # If the requested data are empty, return an empty data frame with the correct shape
  if (nrow(df) == 0) {
    df <- dplyr::mutate(df, lon = numeric(0),
                        lat = numeric(0),
                        Filename = character(0),
                        StartTime = .POSIXct(double(0)),
                        EndTime = .POSIXct(double(0))) %>%
      dplyr::select(-.data$x , -.data$y)
    return(df)
  }
  
  # Get the parameters needed for geolocation
  x_scale <- ncatt_get(nc, "x", "scale_factor")$value
  y_scale <- ncatt_get(nc, "y", "scale_factor")$value
  x_offset <- ncatt_get(nc, "x", "add_offset")$value
  y_offset <- ncatt_get(nc, "y", "add_offset")$value
  r_eq <- ncatt_get(nc, "goes_imager_projection", "semi_major_axis")$value
  r_pol <- ncatt_get(nc, "goes_imager_projection", "semi_minor_axis")$value
  perspective_point <- ncatt_get(nc, "goes_imager_projection",
                                 "perspective_point_height")$value
  H <- perspective_point + r_eq
  lambda0 <- ncatt_get(nc, "goes_imager_projection",
                       "longitude_of_projection_origin")$value * (pi / 180)
  
  # geolocate
  df <- dplyr::mutate(df, x_rad = .data$x * x_scale + x_offset,
                      y_rad = .data$y * y_scale + y_offset) %>%
    dplyr::bind_cols(purrr::map2_dfr(.$x_rad, .$y_rad, goes_lonLat, r_eq,
                                     r_pol, H, lambda0))
  
}

