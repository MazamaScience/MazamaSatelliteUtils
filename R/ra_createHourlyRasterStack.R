#' ra_createHourlyRasterStack.R
#' 
#' @examples
#' \donttest{
#' ### EXTENTS BASED ON PASSED IN FLOATS
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bbox_oregon <- c(-124.56624, -116.46350, 41.99179, 46.29203) # OREGON
#' 
#' rstrStack <- ra_createHourlyRasterStack(
#' satID = "G16", 
#' startTime = "2019-09-06 18", 
#' bbox = bbox_oregon,
#' dqfLevel = 2,
#' res = 0.2)
#' 
#' rasterVis::levelplot(rstrStack)
#' 
#' }

ra_createHourlyRasterStack <- function(
  satID = NULL,
  startTime = NULL,
  var = "AOD",
  res = 0.1,
  bbox = NULL,
  dqfLevel = NULL,
  timezone = 'UTC'
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(satID)
  MazamaCoreUtils::stopIfNull(startTime)
  
  satID <- toupper(satID)
  if ( !(satID %in% c("G16", "G17")) ) {
    stop("Parameter 'satID' must be either 'G16' or 'G17'")
  }
  
  # VALIDATE IS TIME BEING PASSED IN IS ALREADY A POSIX TIME WITH timezone
  time_classes <- c("POSIXct", "POSIXt", "POSIXlt")
  if ( class(startTime)[1] %in% time_classes ) {
    timezone <- attr(startTime,"tzone")
  }
  
  # SINCE WE CAN PASS IN EITHER LOCAL OR UTC, MAKE SURE WE END UP WITH UTC
  #startTime <- MazamaCoreUtils::parseDatetime(datetime = startTime, timezone)
  #startTime <- lubridate::with_tz(startTime, tzone = "UTC")
  
  # ----- Download GOES AOD Files ----------------------------------------------
  ra_downloadAOD(satID, startTime)
  
  # ----- Create List of RasterLayers ------------------------------------------
  
  # create list of AOD raster layers for specified hour and region
  rasterList <- 
    ra_listFiles(satID, startTime) %>%   # create list of filenames for specified hour
    purrr::map(goesaodc_openFile) %>%                       # open each file in the list
    purrr::map(goesaodc_createRaster,                       # rasterize each open file specified params
               res = res,
               bbox = bbox,                             
               dqfLevel = dqfLevel) %>%  
    purrr::map(function(rst) rst[[var]])                # select RasterLayer of specified variable
  
  # ----- Create RasterStack ---------------------------------------------------
  
  # Extents won't match exactly and raster::stack() needs them to, so
  # find the largest extent necessary to encompass all RasterLayers in the
  # list, and apply that new extent to each of the RasterLayers.
  lonLo <- min(purrr::map_dbl(rasterList, function(rst) raster::extent(rst)@xmin))
  lonHi <- max(purrr::map_dbl(rasterList, function(rst) raster::extent(rst)@xmax))
  latLo <- min(purrr::map_dbl(rasterList, function(rst) raster::extent(rst)@ymin))
  latHi <- max(purrr::map_dbl(rasterList, function(rst) raster::extent(rst)@ymax))
  
  ext <- raster::extent(c(lonLo, lonHi, latLo, latHi))
  
  rasterList <- purrr::map(rasterList, function(rst) raster::setExtent(rst, ext))
  
  # now create the stack
  rasterStack <- raster::stack(rasterList)
  
  # assign times to the Z axis
  times <-
    ra_listFiles(satID, startTime) %>%
    purrr::map_chr(goesaodc_getStartString) %>%
    purrr::map(lubridate::parse_date_time, orders = ("YjHMS"))
  
  Z <- purrr::map(times, strftime, format = "%Y%m%d%H%M%S", tz = "UTC")
  
  rasterStack <- raster::setZ(rasterStack, Z)
  # NOTE: Why does this prepend "X" to the names?
  names <- purrr::map(times, strftime, format = "%H:%M:%S", tz = "UTC")
  names(rasterStack) <- names
  
  return(rasterStack)
  
}
