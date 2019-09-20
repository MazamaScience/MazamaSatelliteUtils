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
#' 
#
# ROGER:  Read up on \dontrun{} amd \donttest{} at:
# ROGER:    https://kbroman.org/pkg_primer/pages/tests.html
# ROGER:    https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
# ROGER:
# ROGER:  Also spend some time understanding the use of devtools::test()
# ROGER:
# ROGER:  Update the example below to be \donttest{} and to use one of the
# ROGER:  package internal netcdf files. This should run fast on our desktop
# ROGER:  machines where the goes~Grid files have already been created.
#
#' @examples
#' \dontrun{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' nc <- goesaodc_openFile("OR_ABI-L2-AODC-M6_G16_s20191291201274_e20191291204047_c20191291210009.nc")
#' 
#' tbl <- goesaodc_createTibble(nc)
#' head(tbl)
#' }

goesaodc_createTibble <- function(
  nc
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  # Check that nc has GOES projection
  if ( !goesaodc_isGoesProjection(nc) ) {
    stop("Parameter 'nc' does not have standard GOES-R projection information.")
  }
  
  # ----- Assemble Tibble ------------------------------------------------------
  
  # ROGER:  The goestEastGrid and goestWestGrid are no longer available as
  # ROGER:  package variables. We will need to:
  # ROGER:    1) have satID choose which data file to load
  # ROGER:    2) test for their existence and stop with an appropriate message if not found
  # ROGER:    3) load them into memory with something like:
  # ROGER:  gridFile <- "goestEastGrid.rda"
  # ROGER:  filePath <- file.path(getSatelliteDataDir(), gridFile)
  # ROGER:  goesGrid <- get(load(filePath))
  # ROGER:    4) obtain lon and lat from the now in-memory goesGrid

  varList <- list()
  
  # Get AOD and DQF from netCDF
  varList[["AOD"]] <- as.numeric(ncdf4::ncvar_get(nc, "AOD"))
  varList[["DQF"]] <- as.numeric(ncdf4::ncvar_get(nc, "DQF"))
  
  # Get GOES satellite ID number
  satId <- ncdf4::ncatt_get(nc, varid = 0, attname = "platform_ID")$value
  
  # Read in package internal grid information
  if (satId == "G16") {
    varList[["lon"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$longitude )
    varList[["lat"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$latitude )
  } else if (satId == "G17") {
    varList[["lon"]] <- as.numeric( MazamaSatelliteUtils::goesWestGrid$longitude )
    varList[["lat"]] <- as.numeric( MazamaSatelliteUtils::goesWestGrid$latitude )
  }
  
  # Create a tibble with all columns but removing rows if any of the columns
  # are missing.
  # TODO:  tidyr::drop_na() may be too restrictive if we have multiple data columns.
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
