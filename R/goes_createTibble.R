#' @export
#' 
#' @title Create a tibble from ncdf4 handle
#' 
#' @param nc ncdf4 handle
#' @param vars list of variables
#' 
#' @description Create a tibble with columns: AOD, DQF, longitude and latitude.
#' This information is sufficient to plot as points or create a raster object.
#' 
#' @return Tibble (dataframe) with NetCDF variables and associated locations.

goes_createTibble <- function(
  nc,
  vars = c("AOD", "DQF")
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  # Check that nc has GOES projection
  if ( !isGoesProjection(nc) )
    stop("Parameter 'nc' does not have standard GOES-16 projection information.")

  
  # ----- Assemble Tibble ------------------------------------------------------
  
  varList <- list()
  
  # Read in data from NetCDF file
  for ( var in vars ) {
    varList[[var]] <- as.numeric( ncdf4::ncvar_get(nc, var) )
  }
  
  # Read in package internal grid information
  varList[["longitude"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$longitude )
  varList[["latitude"]] <- as.numeric( MazamaSatelliteUtils::goesEastGrid$latitude )
  
  # TODO:  tidyr::drop_na() may be too restrictive if we have multiple data columns.
  
  # Create a tibble with all columns but removing rows if any of the columns
  # are missing.
  tbl <-
    tibble::as_tibble(varList) %>%
    tidyr::drop_na()
  
  return(tbl)
  
}


# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  filePath <- "/Users/tom/Projects/MazamaSatelliteUtils/local_data/OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc"
  nc <- nc_open(filePath)
  tbl <- goes_createTibble(nc)
  
  # NOTE: The DQF column of the returned tibble contains NAs. However, it looks
  # NOTE: like the rows with NA for DQF also have NA for AOD, so I am going to
  # NOTE: assume that dropping rows with NA DQF is ok downstream. I checked this
  # NOTE: with:
  # NOTE: 
  # NOTE: mask <- is.na(tbl$DQF)
  # NOTE: summary(tbl$AOD[mask])
  # NOTE: Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # NOTE: NA      NA      NA     NaN      NA      NA   24840
  
}