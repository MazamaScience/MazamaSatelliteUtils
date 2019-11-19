#' @export
#' 
#' @title Installs GOES grids in satelliteDataDir
#' @description Creates data files with geolocation information for GOES-16 
#' (East) and GOES-17 (West) satellite products.  Checks for the presence of
#' GOES 16 & GOES 17 .nc (NetCDF) files in directory previously set by 
#' \code{setSatelliteDataDir}. If present, will read projection and 
#' coordinate grid information from them and create GOES East and West grids in 
#' that directory.  If .nc files are missing, will download appropriate ones 
#' and use them to create the grids.
#' 
#' @seealso \code{\link{createGoesGrid}}
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' installGoesGrids()
#' } 

installGoesGrids <- function(
) {
  
  outputDir <- getSatelliteDataDir()
  
  # ---- Check for GOES-16 grid -----------------------------------------------
  
  filename <- "goesEastGrid.rda"
  G16_filepath <- file.path(outputDir, filename)
  
  if ( file.exists(G16_filepath) ) {
    
    message(sprintf("Found %s", G16_filepath))
    
  } else {
    # ---- Download G16 .nc files and create goesEastGrid.rda ------------------ 
    message(sprintf("Creating %s ...", G16_filepath))
    goesaodc_downloadAOD(satID = "G16", 
                         datetime = "201924918",
                         timezone = "UTC",
                         isJulian = TRUE)
    
    nc_file <- goesaodc_listFiles(satID = "G16", 
                                  datetime = "201924918",
                                  timezone = "UTC",
                                  isJulian = TRUE)[1]
    
    nc <- goesaodc_openFile(nc_file)
    createGoesGrid(nc, G16_filepath)
    
    message(paste0("... done!"))
    
  }
  
  # ---- Check for GOES-17 grid -----------------------------------------------
  
  filename <- "goesWestGrid.rda"
  G17_filepath <- file.path(outputDir, filename)
  
  if ( file.exists(G17_filepath) ) {
    
    message(sprintf("Found %s", G17_filepath))
    
  } else {
  # ---- Download G17 .nc files and create goesEastGrid.rda ------------------
    message(sprintf("Creating %s ...", G17_filepath))
    goesaodc_downloadAOD(satID = "G17", 
                         datetime = "201924918",
                         timezone = "UTC",
                         isJulian = TRUE)
    
    nc_file <- goesaodc_listFiles(satID = "G17", 
                                  datetime = "201924918",
                                  timezone = "UTC",
                                  isJulian = TRUE)[1]
    
    nc <- goesaodc_openFile(nc_file)
    createGoesGrid(nc, G17_filepath)
    message(paste0("... done!"))
    
  }
  
  return(invisible(c(G16_filepath, G17_filepath)))
  
}

