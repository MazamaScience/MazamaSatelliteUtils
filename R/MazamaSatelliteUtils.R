#' @import raster
#' @importFrom magrittr '%>%'

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

# ----- Other utility functions ------------------------------------------------

#' @keywords utils
#' @export
#' @title Get raster value at a point
#' @param raster raster layer
#' @param x x (longitude) value
#' @param y y (latitude) value
#' @description Get raster value at a point
#' @return value of raster cell covering the point

getValue <- function(raster, x, y) {
  value <- raster::getValues(raster)[raster::cellFromXY(raster, c(x, y))]
  return(value)
}
