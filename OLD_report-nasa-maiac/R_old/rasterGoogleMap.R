#' @importFrom dismo gmap
#' @export
#' @title Create Google map with raster overlaid
#' @param rasterLayer layer to be overlaid
#' @param breaks set of breaks used to assign colors
#' @param colors a set of colors for different levels of air quality data determined by \code{breaks}
#' @param width width of image, in pixels
#' @param height height of image, in pixels
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level
#' @param maptype map type
#' @param grayscale logical, if TRUE the colored map tile is rendered into a black & white image
#' @param map optional map object returned from \code{monitorGoogleMap(})
#' @param ... arguments passed on to \code{RgoogleMaps::PlotOnStaticMap()} (\emph{e.g.} destfile, cex, pch, etc.)
#' @description Plot raster over Google map tiles
#' @examples 
#' \dontrun{
#' load("~/Projects/airfire-nasa-maiac/localData/Sand_bluesky_CANSAC_6km.RData")
#' maiac <- loadRawMaiac("~/Projects/airfire-nasa-maiac/localData/maiac_sands_AQUA_20160721.nc")
#' ws_raster <- convertGridToRaster(Sand_bluesky_CANSAC_6km)
#' maiacRaster <- convertSwathToRaster(maiac, ws_raster)
#' rasterGoogleMap(maiacRaster, zoom=12)
#' }

# NOTE: base code from the following link:
# NOTE: http://r-sig-geo.2731867.n2.nabble.com/Help-overlaying-raster-on-GoogleMaps-td7026477.html

# TODO: see if there's a better way to reproject without changing the raster data; e.g. project map instead?

rasterGoogleMap <- function(rasterLayer, 
                            breaks=aot_colors$breaks,
                            colors=aot_colors$colors,
                            width=640,
                            height=640,
                            centerLon=NULL,
                            centerLat=NULL,
                            zoom=NULL,
                            maptype='terain',
                            grayscale=TRUE,
                            map=NULL,
                            ...) {
                            
                            # opacity=.6
  
  
  
  # ----- Data Preparation ----------------------------------------------------
  
  if ( is.null(centerLon) ) {
    centerLon <- base::mean(rasterLayer@extent[1:2])
  }
  
  if ( is.null(centerLat) ) {
    centerLat <- base::mean(rasterLayer@extent[3:4])
  }
  
  # Guess at zoom level if not specified
  if ( is.null(zoom) ) {
    maxRange <- max( diff(range(rasterLayer@extent[1:2], na.rm=TRUE)), diff(range(rasterLayer@extent[3:4], na.rm=TRUE)) )
    if ( maxRange > 50 ) {
      zoom <- 3
    } else if ( maxRange > 20 ) {
      zoom <- 4
    } else if ( maxRange > 10 ) {
      zoom <- 5
    } else if ( maxRange > 5 ) {
      zoom <- 6
    } else if ( maxRange > 2 ) {
      zoom <- 7
    } else if ( maxRange > 1 ) {
      zoom <- 8
    } else if ( maxRange > 0.5 ) {
      zoom <- 9
    } else {
      zoom <- 9
    }
  } else {
    zoom <- round(zoom)
  }
  
  
  # dog <- plotmap(centerLat, centerLon, zoom=7, maptype = "terrain")
  
  if ( is.null(colors) ) {
    colors <- aot_colors$colors[length(aot_colors$colors)]
  }
  if ( is.null(breaks) ) {
    breaks <- aot_colors$breaks
  }
  
  colors[which(colors=="white")] <- "#FFFFFF"
  
  # attempt 1
  map <- dismo::gmap(rasterLayer)
  plot(map)
  r2 <- raster::projectRaster(rasterLayer, map)
  plot(r2, add=TRUE, alpha=.75, col=colors, breaks=breaks, legend=FALSE)
  
  
  
  # # attempt 2
  # map <- dismo::gmap(rasterLayer)
  # map_new <- raster::projectRaster(map, rasterLayer)
  # plot(map_new)
  
  
  
  
  # 
  # if ( is.null(map) ) {
  #   map <- RgoogleMaps::GetMap(center=c(centerLat,centerLon), size=c(height,width),
  #                              zoom=zoom, maptype=maptype, GRAYSCALE=grayscale);
  #   map <- RgoogleMaps::PlotOnStaticMap(map)
  # }
  # 
  # 
  # 
  # # Overlay function default arguments ----------------------------------------
  # 
  # argsList <- list(...)
  # 
  # # Explicitly declare defaults for the PlotOnStaticMap() function
  # argsList$MyMap <- map
  # argsList$lat <- centerLat
  # argsList$lon <- centerLon
  # argsList$size <- map$size
  # argsList$add=TRUE
  # argsList$FUN <- ifelse('FUN' %in% names(argsList), argsList$FUN, points)
  # argsList$pch <- ifelse('pch' %in% names(argsList), argsList$pch, 16)
  # # "ifelse returns a value with the same shape as test ..."
  # if ( ! 'col' %in% names(argsList) ) {
  #   argsList$col <- cols
  # }
  # argsList$cex <- ifelse('cex' %in% names(argsList), argsList$cex, par("cex")*2.0)
  # 
  # map <- do.call(RgoogleMaps::PlotOnStaticMap, argsList)
  # 
  # return(invisible(map)) 
  
  # return (c(centerLon, centerLat))
  
  # return(dog)
  
  
  
}
