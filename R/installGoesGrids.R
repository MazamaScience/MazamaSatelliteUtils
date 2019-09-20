#' @export
#' 
#' @title Create GOES grids in satelliteDataDir
#' @description Creates data files with geolocation information for GOES-16 
#' (East) and GOES-17 (West) satellite products.
#' 
#' ROGER: Flesh out this documentation
#'  
installGoesGrids <- function(
  # ROGER:  Optional "showProgress = TRUE"
) {
  
  # ROGER:  Can we use the progress package to display a progress bar??
  # ROGER:    https://github.com/r-lib/progress
  # ROGER:
  # ROGER:  The goal is only to let someone know that work is being done and
  # ROGER:  show elapsed time.
  
  outputDir <- getSatelliteDataDir()
  
  # ----- GOES-16 --------------------------------------------------------------
  
  filename <- "goesEastGrid.rda"
  G16_filepath <- file.path(outputDir, filename)
  
  if ( !file.exists(G16_filepath) ) {
    
    # Get a NetCDF handle for the package internal GOES-16 dataset
    nc_filepath <- system.file(
      "extdata", 
      "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc", 
      package = "MazamaSatelliteUtils"
    )
    nc <- ncdf4::nc_open(nc_filepath)
    
    # Get the projection information
    projection <- goesaodc_getProjection(nc)
    
    # Get the grid dimensions
    nrow <- length(ncvar_get(nc, varid = "x"))
    ncol <- length(ncvar_get(nc, varid = "y"))
    
    # Create a tibble where each AOD value has an associated location
    coordGrid <- goesaodc_getCoordGrid(nc)
    longitude <- coordGrid$lon
    latitude <- coordGrid$lat
    
    # Create a list with longitude and latitude matrices
    goesEastGrid <- list(
      longitude = matrix(longitude, nrow = nrow, ncol = ncol),
      latitude = matrix(latitude, nrow = nrow, ncol = ncol), 
      projection = projection
    )  
    
    save(goesEastGrid, file = G16_filepath)
    
  }
  
  # ----- GOES-17 --------------------------------------------------------------
  
  filename <- "goesWestGrid.rda"
  G17_filepath <- file.path(outputDir, filename)
  
  if ( !file.exists(G17_filepath) ) {
    
    # Get a NetCDF handle for the package internal GOES-17 dataset
    nc_filepath <- system.file(
      "extdata", 
      "OR_ABI-L2-AODC-M6_G17_s20192491826196_e20192491828569_c20192491830494.nc", 
      package = "MazamaSatelliteUtils"
    )
    nc <- ncdf4::nc_open(nc_filepath)
    
    # Get the projection information
    projection <- goesaodc_getProjection(nc)
    
    # Get the grid dimensions
    nrow <- length(ncvar_get(nc, varid = "x"))
    ncol <- length(ncvar_get(nc, varid = "y"))
    
    # Create a tibble where each AOD value has an associated location
    coordGrid <- goesaodc_getCoordGrid(nc)
    longitude <- coordGrid$lon
    latitude <- coordGrid$lat
    
    # Create a list with longitude and latitude matrices
    goesWestGrid <- list(
      longitude = matrix(longitude, nrow = nrow, ncol = ncol),
      latitude = matrix(latitude, nrow = nrow, ncol = ncol), 
      projection = projection
    ) 
    
    save(goesWestGrid, file = G17_filepath)
    
  }
  
  return(invisible(c(G16_filepath, G17_filepath)))
  
}

