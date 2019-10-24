#' @export
#' 
#' @title Create a tibble from ncdf4 handle
#' 
#' @param nc ncdf4 handle
#' 
#' @description Create a tibble with columns: AOD, DQF, lon and lat.
#' This information is sufficient to plot as points or create a raster object.
#' 
#' @return Tibble (dataframe) with NetCDF variables and associated locations.
#
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#'
#' setSatelliteDataDir("~/Data/Satellite")
#'
#' ncFile <- system.file(
#'   "extdata", 
#'   "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc", 
#'   package = "MazamaSatelliteUtils"
#' )
#'                      
#' nc <- goesaodc_openFile(ncFile)
#'
#' tbl <- goesaodc_createTibble(nc)
#' head(tbl)
#' }

goesaodc_createTibble <- function(
  nc = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(nc)
  
  # Check that nc has GOES projection
  if ( !goesaodc_isGoesProjection(nc) ) {
    stop("Parameter 'nc' does not have standard GOES-R projection information.")
  }
  
  # ----- Get grid data --------------------------------------------------------
  
  # Get satID from netCDF, will be either "G16" or "G17"
  satID <- ncdf4::ncatt_get(nc, varid = 0, attname = "platform_ID")$value
  
  # Choose which gridFile to load based on satID
  if ( satID == "G16") {
    gridFile <- "goesEastGrid.rda"
  } else if ( satID == "G17" ) {
    gridFile <- "goesWestGrid.rda"
  }
  
  # Assemble the correct filepath based on satID and Data directory
  filePath <- file.path(getSatelliteDataDir(), gridFile)
  
  # Test for grid existence and if found, load it. Stop with appropriate message
  # if missing
  if ( file.exists(filePath) ) {
    goesGrid <- get(load(filePath))
  } else {
    stop("Grid file not found. Run 'installGoesGrids()' first")
  }  
  
  # ----- Create tibble --------------------------------------------------------
  
  varList <- list()
  
  # Get lon and lat from grid file
  varList[["lon"]] <- as.numeric( goesGrid$longitude )
  varList[["lat"]] <- as.numeric( goesGrid$latitude )
  
  # Get AOD and DQF from netCDF
  varList[["AOD"]] <- as.numeric(ncdf4::ncvar_get(nc, "AOD"))
  varList[["DQF"]] <- as.numeric(ncdf4::ncvar_get(nc, "DQF"))
  
  # Create a tibble with all columns but removing rows if any of the columns
  # are missing.
  # TODO:  tidyr::drop_na() may be too restrictive for multiple data columns.
  tbl <-
    tibble::as_tibble(varList) %>%
    tidyr::drop_na()
  
  return(tbl)
  
}

# NOTE: The DQF column of the returned tibble contains NAs. However, it looks
# NOTE: like the rows with NA for DQF also have NA for AOD, so I am going to
# NOTE: assume that dropping rows with NA DQF is ok downstream. I checked this
# NOTE: with:
# NOTE: 
# NOTE: mask <- is.na(tbl$DQF)
# NOTE: summary(tbl$AOD[mask])
# NOTE: Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# NOTE: NA      NA      NA     NaN      NA      NA   24840
