#' @import raster
#' @importFrom magrittr '%>%'
#'
#' @docType package
#' @name MazamaSatelliteUtils
#' @title Utilities for working with model and satellite data as rasters.
#' @description A suite of utility functions providing functionality commonly 
#' needed for download of satellite products and conversion to Raster objects.
#' 
#' @section GOES AOD:
#' ABI L2+ AOD NetCDF files from NOAA contain the aerosol optical 
#' depth at 550 nm over land and over water, associated quality flags, mean, 
#' maximum, minimum and standard deviation of 550-nm AOD for the entire domain. 
#' The AOD is a measure of the columnar extinction (scattering +absorption) of 
#' radiation by aerosols. It is proportional to the amount (number or mass 
#' concentration) of aerosols in an atmospheric column. 
#' \describe{
#'   \item{dqf}{data quality}
#'   \item{aod}{measure optical depth}
#'   ...
#' }
#' 
#' @source \url{https://www.ncdc.noaa.gov/sites/default/files/attachments/GOES-17_ABI_L2_AOD_Provisional_ReadMe.pdf}
#' 
#' The package contains two sample NetCDF files with GOES-16 and GOES-17 AOD 
#' data from for 2019-09-06 18:26 UTC -- 1pm Pacific Daily Time.
#'
#' Access these files with:
#' 
#' \preformatted{
#' G16_filepath <- system.file(
#'   "extdata", 
#'   "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc", 
#'   package = "MazamaSatelliteUtils")
#' 
#' G17_filepath <- system.file(
#'   "extdata", 
#'   "OR_ABI-L2-AODC-M6_G17_s20192491826196_e20192491828569_c20192491830494.nc", 
#'   package = "MazamaSatelliteUtils")
#' }
#' 
NULL


# ----- Example datasets ------------------------------------------------------

#' @docType data
#' @keywords datasets
#' @name goesEastGrid
#' @title GOES East Grid
#' @format A list with three elements
#' @description
#' This dataset is the latitude/longitude grid that GOES-16 ABI data is defined
#' on, computed from ABI scan angles provided in the netCDF and the projection
#' parameters found in \code{goesEastGrid$projection}. Because GOES-16 is a
#' geostationary satellite, the projection information should not change from
#' scan to scan. Using this dataset saves us from repeating the computationally
#' expensive task of converting scan angles to latitude/longitude. 
NULL

#' @docType data
#' @keywords datasets
#' @name goesWestGrid
#' @title GOES West Grid
#' @format A list with three elements
#' @description
#' This dataset is the latitude/longitude grid that GOES-17 ABI data is defined
#' on, computed from ABI scan angles provided in the netCDF and the projection
#' parameters found in \code{goesEastGrid$projection}. Because GOES-17 is a
#' geostationary satellite, the projection information should not change from
#' scan to scan. Using this dataset saves us from repeating the computationally
#' expensive task of converting scan angles to latitude/longitude. 
NULL

# ----- Internal Package State -------------------------------------------------

MazamaSatelliteUtilsEnv <- new.env(parent = emptyenv())
MazamaSatelliteUtilsEnv$dataDir <- NULL

# ----- Data Directory Configuration -------------------------------------------

#' @docType data
#' @keywords environment
#' @name SatelliteDataDir
#' @title Directory for Satellite Data
#' @format Absolute path string.
#' @description This package maintains an internal directory location which 
#' users can set using \code{setSatelliteDataDir()}. All package functions use 
#' this directory whenever datasets are created or loaded.
#' 
#' The default setting when the package is loaded is \code{getwd()}.
#' @seealso \link{getSatelliteDataDir}
#' @seealso \link{setSatelliteDataDir}
NULL

#' @keywords environment
#' @export
#' @title Get Satellite Data Directory
#' @description Returns the data directory where Satellite data is located.
#' @return Absolute path string.
#' @seealso \link{SatelliteDataDir}
#' @seealso \link{setSatelliteDataDir}

getSatelliteDataDir <- function() {
  if (is.null(MazamaSatelliteUtilsEnv$dataDir)) {
    stop(paste0('No data directory found. Please set a data directory with ',
                'setSatelliteDataDir("YOUR_DATA_DIR")'),
         call.=FALSE)
  } else {
    return(MazamaSatelliteUtilsEnv$dataDir)
  }
}

#' @keywords environment
#' @export
#' @title Set Satellite Data Directory
#' @param dataDir directory where Satellite datasets are stored
#' @description Sets the data directory where Satellite data is located.
#' If the directory does not exist, it will be created.
#' @return Silently returns previous value of data directory.
#' @seealso \link{SatelliteDataDir}
#' @seealso \link{getSatelliteDataDir}

setSatelliteDataDir <- function(dataDir) {
  old <- MazamaSatelliteUtilsEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    MazamaSatelliteUtilsEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setSatelliteDataDir(",dataDir,")."))
  })
  return(invisible(old))
}

