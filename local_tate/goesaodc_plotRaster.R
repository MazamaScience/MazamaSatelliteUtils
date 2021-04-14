#'
#' 
#' @title Create a plot of a GOES AOD Raster
#' 
#' @description Quickly subsamples and plots points in a GOES AOD 
#' spatialPointsDataFrame
#' 
#' @param raster Raster object
#' @param paletteName RColorBrewer palette name; Defaults to 'Reds'.
#' @param bbox Bounding box for the region of interest; Defaults to Raster extent.
#' @param title Plot title label.
#' 
#' @examples 
#' \donttest{
#' library(MazamaSatelliteUtils)
#' 
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' ncFile <- goesaodc_downloadAOD(
#'   satID = "G17", 
#'   datetime = "2020-09-16 12",
#'   timezone = "America/Los_Angeles",
#'   verbose = TRUE 
#' )[1]
#'
#' nc <- goesaodc_openFile(ncFile)
#' oregonBbox <- c(-125, -118, 41, 47)
#' 
#' raster <- goesaodc_createRaster(
#'   nc = nc,
#'   bbox = oregonBbox,
#'   res = 0.05,
#'   dqfLevel = 3
#' )
#' 
#' goesaodc_plotRaster(
#'   raster = raster,
#'   palette = "Reds",
#'   title = paste0("AOD: ", goesaodc_convertFilenameToDatetime(ncFile))
#' )
#' }

goesaodc_plotRaster <- function(
  raster = NULL,
  palette = "Reds",
  bbox = NULL,
  title = ""
) {
  
  if ( is.null(bbox) ) {
    extent <- raster::extent(raster)
    bbox <- bboxToVector(extent)
  }
  
  xlim <- c(bbox[1], bbox[2])
  ylim <- c(bbox[3], bbox[4])
  
  AirFirePlots::plot_composite(
    raster = raster,
    states = TRUE,
    bgName = "AOD",
    bgRasterBreaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
    bgRasterPalette = palette,
    xlim = xlim,
    ylim = ylim,
    expand = FALSE,
    title = title,
    flab = "AOD"
  )  
  
}
