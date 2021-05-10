#' @export
#' 
#' @title Create a GOES grid in satelliteDataDir
#' 
#' @description Creates data files with geolocation information for GOES-16 
#' (East) or GOES-17 (West) satellite products. Takes an open .nc filehandle 
#' and reads projection and coordinate grid information from it to create
#' a GOES East or West grid in the directory previously set with 
#' \code{setSatelliteDataDir()}.
#' 
#' @param nc ncdf4 handle.
#' @param grid_filepath filepath location for either goesEastGrid.rda or 
#' goesWestGrid.rda.
#' 
#' @return Invisibly returns the file path of the created GOES grid file.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' scanFilename <- goesaodc_listScanFiles(
#'   satID = "G16", 
#'   datetime = "2019-9-6 18:00",
#'   timezone = "UTC",
#' )
#'    
#' nc <- goesaodc_openFile(scanFilename)
#' G16_filepath <- file.path(getSatelliteDataDir(), "goesEastGrid.rda")
#' 
#' createGoesGrid(nc, G16_filepath)
#' } 

createGoesGrid <- function (
  nc = NULL,
  grid_filepath = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(grid_filepath)
  
  # ----- Create grid ----------------------------------------------------------
  
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
  goesGrid <- list(
    longitude = matrix(longitude, nrow = nrow, ncol = ncol),
    latitude = matrix(latitude, nrow = nrow, ncol = ncol), 
    projection = projection
  )
  
  # ----- Save grid ------------------------------------------------------------
  
  save(goesGrid, file = grid_filepath)
  
  # ----- Return ---------------------------------------------------------------

  return(invisible(c(grid_filepath)))
  
}