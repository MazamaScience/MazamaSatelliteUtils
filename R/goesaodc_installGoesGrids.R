#' @export
#' 
#' @title Install GOES grids
#' 
#' @description Creates data files with geolocation information for GOES-16 
#' (East) and GOES-17 (West) satellite products.  Checks for the presence of
#' GOES 16 & GOES 17 .nc (NetCDF) files in the directory previously set by 
#' \code{setSatelliteDataDir}. If present, will read projection and 
#' coordinate grid information from them and create GOES East and West grids in 
#' that directory. If .nc files are missing, will download appropriate ones 
#' and use them to create the grids.
#' 
#' @seealso \code{\link{goesaodc_createGoesGrid}}
#' 
#' @return Invisible vector of local GOES 16 and 17 grid filepaths.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' goesaodc_installGoesGrids()
#' } 

goesaodc_installGoesGrids <- function(
) {
  
  outputDir <- getSatelliteDataDir()
  
  # ----- Check for GOES-16 grid -----------------------------------------------
  
  filename <- "goesEastGrid.rda"
  G16_filepath <- file.path(outputDir, filename)
  
  if ( file.exists(G16_filepath) ) {
    
    message(sprintf("Found %s", G16_filepath))
    
  } else {
    
    # ----- Download G16 .nc files and create goesEastGrid.rda -----------------
    
    message(sprintf("Creating %s ...", G16_filepath))
    goesaodc_downloadScanFiles(
      satID = "G16", 
      datetime = "2019-09-06 18:00",
      timezone = "UTC"
    )
    
    scanFile <- goesaodc_listScanFiles(
      satID = "G16", 
      datetime = "2019-09-06 18:00",
      timezone = "UTC"
    )
    
    nc <- goesaodc_openScanFile(scanFile)
    goesaodc_createGoesGrid(nc, G16_filepath)
    
    message(paste0("... done!"))
    
  }
  
  # ----- Check for GOES-17 grid -----------------------------------------------
  
  filename <- "goesWestGrid.rda"
  G17_filepath <- file.path(outputDir, filename)
  
  if ( file.exists(G17_filepath) ) {
    
    message(sprintf("Found %s", G17_filepath))
    
  } else {
    
    # ----- Download G17 .nc files and create goesWestGrid.rda -----------------
    
    message(sprintf("Creating %s ...", G17_filepath))
    goesaodc_downloadScanFiles(
      satID = "G17", 
      datetime = "2019-09-06 18:00",
      timezone = "UTC"
    )
    
    scanFile <- goesaodc_listScanFiles(
      satID = "G17", 
      datetime = "2019-09-06 18:00",
      timezone = "UTC"
    )
    
    nc <- goesaodc_openScanFile(scanFile)
    goesaodc_createGoesGrid(nc, G17_filepath)
    message(paste0("... done!"))
    
  }
  
  return(invisible(c(G16_filepath, G17_filepath)))
  
}


#' @title Create a GOES grid in satelliteDataDir
#' 
#' @description Creates data files with geolocation information for GOES-16 
#' (East) or GOES-17 (West) satellite products. Takes an open .nc filehandle 
#' and reads projection and coordinate grid information from it to create
#' a GOES East or West grid in the directory previously set with 
#' \code{setSatelliteDataDir()}.
#' 
#' @param nc ncdf4 handle.
#' @param gridFilepath filepath for either goesEastGrid.rda or 
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
#'   datetime = "2019-09-06 18:00",
#'   timezone = "UTC",
#' )
#'    
#' nc <- goesaodc_openScanFile(scanFilename)
#' G16_filepath <- file.path(getSatelliteDataDir(), "goesEastGrid.rda")
#' 
#' MazamaSatelliteUtils:::goesaodc_createGoesGrid(nc, G16_filepath)
#' } 

goesaodc_createGoesGrid <- function (
  nc = NULL,
  gridFilepath = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  MazamaCoreUtils::stopIfNull(gridFilepath)
  
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
  
  save(goesGrid, file = gridFilepath)
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(c(gridFilepath)))
  
}