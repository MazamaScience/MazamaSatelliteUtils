#' @import raster
#' @importFrom magrittr '%>%'

# ----- Internal Package State --------------------------------------------------------

NasaMaiacEnv <- new.env(parent = emptyenv())
NasaMaiacEnv$dataDir <- NULL

# ----- Data Directory Configuration --------------------------------------------------

#' @docType data
#' @keywords environment
#' @name MaiacDataDir
#' @title Directory for MAIAC Data
#' @format Absolute path string.
#' @description This package maintains an internal directory location which users can set
#' using \code{setMaiacDataDir()}. All package functions use this directory whenever datasets
#' are created or loaded.
#' 
#' The default setting when the package is loaded is \code{getwd()}.
#' @seealso \link{getMaiacDataDir}
#' @seealso \link{setMaiacDataDir}
NULL

#' @keywords environment
#' @export
#' @title Get MAIAC Data Directory
#' @description Returns the data directory where MAIAC data is located.
#' @return Absolute path string.
#' @seealso \link{MaiacDataDir}
#' @seealso \link{setMaiacDataDir}

getMaiacDataDir <- function() {
  if (is.null(NasaMaiacEnv$dataDir)) {
    stop("No data directory found. Please set a data directory with setMaiacDataDir('~/Data/MAIAC')",call.=FALSE)
  } else {
    return(NasaMaiacEnv$dataDir)
  }
}

#' @keywords environment
#' @export
#' @title Set MAIAC Data Directory
#' @param dataDir directory where MAIAC datasets are stored
#' @description Sets the data directory where MAIAC data is located.
#' If the directory does not exist, it will be created.
#' @return Silently returns previous value of data directory.
#' @seealso \link{MaiacDataDir}
#' @seealso \link{getMaiacDataDir}

setMaiacDataDir <- function(dataDir) {
  old <- NasaMaiacEnv$dataDir
  dataDir <- path.expand(dataDir)
  tryCatch({
    if (!file.exists(dataDir)) dir.create(dataDir)
    NasaMaiacEnv$dataDir <- dataDir
  }, warning = function(warn) {
    warning("Invalid path name.")
  }, error   = function(err) {
    stop(paste0("Error in setMaiacDataDir(",dataDir,")."))
  })
  return(invisible(old))
}

# ----- Other utility functions -------------------------------------------------------

#' @keywords utils
#' @export
#' @title Convert column-integrated PM2.5 values to AOD
#' @param pm25 vector of column-integrated pm25 values
#' @description Converts a vector of PM2.5 values to AOD values
#' @return vector of AOD values.

toAOD <- function(pm25) {
  bext <- 2.5 * 10^(-6) * pm25
  aod <- bext * 10000
  return(aod)
}

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
