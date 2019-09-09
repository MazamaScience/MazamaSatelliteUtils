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
    stop("Parameter 'nc' does not have standard GOES-16 projection information.")
  }
  
  # ----- Assemble Tibble ------------------------------------------------------
  
  varList <- list()
  
  # Get AOD and DQF from netCDF
  varList[["AOD"]] <- as.numeric(ncdf4::ncvar_get(nc, "AOD"))
  varList[["DQF"]] <- as.numeric(ncdf4::ncvar_get(nc, "DQF"))
  
  # Get satellite GOES ID number
  i <- regexpr("_G[0-9]+_", nc$filename)
  satId <- substr(nc$filename, i + 2, i + attributes(i)$match.length - 2)[1]
  
  # Read in package internal grid information
  if (satId == 16) {
    varList[["lon"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$longitude )
    varList[["lat"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$latitude )
  } else if (satId == 17) {
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
