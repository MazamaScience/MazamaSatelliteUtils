# Code refactored from Sean Raffuse's goesfire repository:
#   https://github.com/raffscallion/goesfire

#' @export
#' @importFrom ncdf4 ncvar_get
#'
#' @title Determines the x and y bounds (in radians) for a given netcdf handle
#'
#' @param nc ncdf4 handle.
#'
#' @return A named list with x1, x2, y1, and y2.

goesaodc_getCoordBounds <- function(
  nc = NULL
) {

  MazamaCoreUtils::stopIfNull(nc)
    
  if ( class(nc) != "ncdf4" ) {
    stop("argument 'nc' must be an ncdf4 object")
  }
  
  x_bounds <- ncvar_get(nc, "x_image_bounds")
  y_bounds <- ncvar_get(nc, "y_image_bounds")
  
  bounds <- list(
    x1 = x_bounds[1], 
    x2 = x_bounds[2], 
    y1 = y_bounds[1], 
    y2 = y_bounds[2]
  )
  
  return(bounds)
  
}


#' @export
#' @importFrom ncdf4 ncatt_get
#'
#' @title Creates a data frame of decimal degree lon/lat pairs for a given
#' netcdf handle
#'
#' @param nc ncdf4 handle.
#'
#' @description Code borrowed from 
#' https://github.com/raffscallion/goesfire/blob/master/R/utils.R
#'
#' @return Dataframe

goesaodc_getCoordGrid <- function(
  nc
) {
  
  MazamaCoreUtils::stopIfNull(nc)
  
  if ( class(nc) != "ncdf4" ) {
    stop("argument 'nc' must be an ncdf4 object")
  }
  
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
  
  df <- 
    dplyr::tibble(
      x = as.vector(x_array),
      y = as.vector(y_array)
    ) %>%
    dplyr::mutate(
      x_rad = x * x_scale + x_offset,
      y_rad = y * y_scale + y_offset
    )
  
  # Get the parameters needed for geolocation
  r_eq <- ncatt_get(nc, "goes_imager_projection", "semi_major_axis")$value
  r_pol <- ncatt_get(nc, "goes_imager_projection", "semi_minor_axis")$value
  perspective_point <- ncatt_get(nc, "goes_imager_projection",
                                 "perspective_point_height")$value
  H <- perspective_point + r_eq
  lambda0 <- ncatt_get(
    nc,
    "goes_imager_projection",
    "longitude_of_projection_origin"
  )$value * (pi / 180)
  
  # ----- Convert scan angles to latitude and longitude ------------------------
  
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
  
  goesaodc_lonLat <- function(x, y, r_eq, r_pol, H, lambda0) {
    
    # Calculate distance from satellite to point of interest
    a <- sin(x)^2 + cos(x)^2 * (cos(y)^2 + (r_eq^2 / r_pol^2) * sin(y)^2)
    b <- -2 * H * cos(x) * cos(y)
    c <- H^2 - r_eq^2
    suppressWarnings( r_s <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a) )
    
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
  df <- dplyr::bind_cols(
    df, 
    purrr::map2_dfr(
      df$x_rad, df$y_rad,       # variables
      goesaodc_lonLat,          # function
      r_eq, r_pol, H, lambda0)  # constants
    )
  
  return(df)
  
}


#' @export
#' 
#' @title Get projection metadata from GOES-R ABI ncdf4 object
#' 
#' @description Return the projection information of `nc`.
#' 
#' @param nc ncdf4 handle.
#' 
#' @return list of metadata

goesaodc_getProjection <- function(
  nc = NULL
) {
  
  MazamaCoreUtils::stopIfNull(nc)
  
  if ( class(nc) != "ncdf4" ) {
    stop("argument 'nc' must be an ncdf4 object")
  }
  
  projection <- ncdf4::ncatt_get(nc, "goes_imager_projection")
  return(projection)
  
}

#' @export
#' 
#' @title Check if `nc` projection is GOES projection
#' 
#' @description Determines if projection information in `nc` matches GOES 
#' projection information.
#' 
#' @param nc ncdf4 handle.
#' 
#' @return logical

goesaodc_isGoesProjection <- function(
  nc = NULL
) {
  
  MazamaCoreUtils::stopIfNull(nc)
  
  if ( class(nc) != "ncdf4" ) {
    stop("argument 'nc' must be an ncdf4 object")
  }
  
  projection <- goesaodc_getProjection(nc)
  satelliteDataDir <- getSatelliteDataDir()
  
  tryCatch(
    expr = {
      goesEastGrid <- get(load(file.path(satelliteDataDir, "goesEastGrid.rda")))
      goesWestGrid <- get(load(file.path(satelliteDataDir, "goesWestGrid.rda")))
      isGoesEast <- all(unlist(projection) == unlist(goesEastGrid$projection))
      isGoesWest <- all(unlist(projection) == unlist(goesWestGrid$projection))
    },
    error = function(e){
      stop(e)
    },
    warning = function(w){
      stop(w)
    }
  )  
  
  return(isGoesEast || isGoesWest)
  
}


#' @export
#' 
#' @title Get the scan start time from a GOES AODC netCDF file name
#' 
#' @description Gets the scan start time from a GOES AODC netCDF file name.
#' 
#' @param file File name of GOES AOD file.
#' 
#' @return The scan start time.

goesaodc_convertFilenameToDatetime <- function(
  file = NULL
) {
  
  MazamaCoreUtils::stopIfNull(file)
  
  filePattern <- "OR_ABI-L2-AODC-M[0-9]_G(16|17)_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
  
  if ( regexpr(filePattern, file) == -1 ) {
    stop("argument 'file' does not match expected file name format")
  }
  
  # Example file name:
  #  OR_ABI-L2-AODC-M4_G17_s20192481500215_e20192481505115_c20192481507046.nc
  
  # Extract the "s..." part and strip of the 's' and the fractional seconds 
  # to get a nicely parseable "YjHMS".
  
  start_string <-
    stringr::str_split_fixed(file, "_", 6)[,4] %>%
    stringr::str_sub(2, -2)
  
  start_time <- MazamaCoreUtils::parseDatetime(
    start_string,
    timezone = "UTC",
    isJulian = TRUE
  )
  
  return(start_time)
  
}


#' @export
#'
#' @title Converts raw, signed short AOD values from GOES 16 & 17 NetCDF files
#' 
#' @description Performs a series of conversions to raw AOD values from .nc file
#' #' \itemize{
#' \item{0}{ -- Converts \code{_FillValue} entries to \code{NA}}
#' \item{1}{ -- Converts signed short values into unsigned short values}
#' \item{2}{ -- Applies \code{scale_factor} and \code{aod_offset} to create
#' correctly scaled AOD values}
#' }
#' 
#' @param aod_data raw, signed short AOD data read in from GOES .nc file
#' @param aod_attributes AOD metadata read in from GOES .nc file
#' 
#' @note As of ncdf4 version 1.16.1, the signedbyte flag is only used to
#' interpret singlye byte values. GOES AODC values are written as unsigned short 
#' int but \code{ncdf4::ncvar_get()} interprets these 16 bits as signed short 
#' int. Hence the need for conversion.
#' 
#' @return Matrix of properly scaled AOD values.

goesaodc_scaleAOD <- function(
  aod_data,
  aod_attributes
) {
  
  # > str(aod_attributes)
  # List of 13
  # $ _FillValue         : int -1
  # $ long_name          : chr "ABI L2+ Aerosol Optical Depth at 550 nm"
  # $ standard_name      : chr "atmosphere_extinction_optical_thickness_due_to_ambient_aerosol"
  # $ _Unsigned          : chr "true"
  # $ valid_range        : int [1:2] 0 -6
  # $ scale_factor       : num 7.71e-05
  # $ add_offset         : num -0.05
  # $ units              : chr "1"
  # $ resolution         : chr "y: 0.000056 rad x: 0.000056 rad"
  # $ coordinates        : chr "sunglint_angle retrieval_local_zenith_angle quantitative_local_zenith_angle retrieval_solar_zenith_angle quanti"| __truncated__
  # $ grid_mapping       : chr "goes_imager_projection"
  # $ cell_methods       : chr "sunglint_angle: point (no pixel produced over sea only) retrieval_local_zenith_angle: point (good or degraded q"| __truncated__
  # $ ancillary_variables: chr "DQF"
  
  # NOTE:  In the conversion of signed short int into unsigned short int, the
  # NOTE:  first bit of 16 is now used to add positive 2^15 rather than negative
  # NOTE:  2^15. So we need to add 2^15 twice, once to undo it's previous use 
  # NOTE:  to add negative 2^15 and once more in its new use as the 2^15 place.
  # NOTE:  (The bit count starts at zero because 2^0 is the "ones" place.)
  
  conversion <- 2^15 + 2^15
  
  # Convert the negative values to proper unsigned short values.
  negative_mask <- aod_data < 0
  aod_data[negative_mask] <- aod_data[negative_mask] + conversion
  
  # Convert (modified) fill_values to NA
  fill_value_mask <- aod_data == (aod_attributes$'_FillValue' + conversion)
  aod_data[fill_value_mask] <- NA
  
  # Scale AOD data into proper range
  scaled_aod <- (aod_data * aod_attributes$scale_factor) + aod_attributes$add_offset
  
  return(scaled_aod)
  
}
