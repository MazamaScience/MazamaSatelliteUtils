#' @export
#' @importFrom leaflet %>%
#' @title Create leaflet map with raster overlaid
#' @param rasterLayer layer to be overlaid
#' @param opacity raster transparency
#' @param colors colors to use in plot of raster
#' @param breaks breaks to use in plot of raster
#' @param style underlying map style
#' @param legendTitle title for legend
#' @description Plot raster over interactive map
#' @examples 
#' \dontrun{
#' load("~/Projects/airfire-nasa-maiac/localData/Sand_bluesky_CANSAC_6km.RData")
#' maiac <- loadRawMaiac("~/Projects/airfire-nasa-maiac/localData/maiac_sands_AQUA_20160721.nc")
#' ws_raster <- convertGridToRaster(Sand_bluesky_CANSAC_6km)
#' maiacRaster <- convertSwathToRaster(maiac, ws_raster)
#' rasterLeaflet(maiacRaster)
#' rasterLeaflet(ws_raster[[76]], colors=bs_colors$colors, breaks=bs_colors$breaks, legendTitle="PM2.5 Concentration")
#' }

# https://rstudio.github.io/leaflet/raster.html

# # temp for testing
# if ( FALSE ) {
# 
#   source("./R/createSandFiresData.R")
# 
#   ws_monitor <- Sand_monitors
#   rasterLayer <- maiacRaster
# 
#   # colors <- c("#0C2C84", "#41B6C4", "#FFFFCC")
#   # colors <- AQI$colors
#   rasterLeaflet(rasterLayer, .6)
# 
# }

rasterLeaflet <- function(rasterLayer, opacity=.7, colors=aot_colors$colors, breaks=aot_colors$breaks, style=NULL, legendTitle="AOT") {
  
  # map <- PWFSLSmoke::monitorLeaflet(ws_monitor)
  # map <- leaflet::addTiles(map, urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}")
  # leaflet::addRasterImage(map, rasterLayer, opacity = opacity, colors = colors)
  # pal <- leaflet::colorNumeric(colors, values(rasterLayer), na.color = "transparent")
  # leaflet::addRasterImage(map, rasterLayer, col=pal, opacity = opacity)
  
  # TODO: add functionality for more layer types, as in PWFSLSmoke::monitorLeaflet
  providerTiles <- ifelse(is.null(style), "CartoDB.Positron", style)
  # NOTE: other options include:
  # NOTE:  - "Esri.WorldTopoMap"
  # NOTE:  - ...
  
  lonRange <- rasterLayer@extent[1:2]
  latRange <- rasterLayer@extent[3:4]
  zoom <- 7
  
  m <- leaflet::leaflet() %>%
    leaflet::setView(lng=mean(lonRange), lat=mean(latRange), zoom=zoom) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addRasterImage(rasterLayer, opacity = opacity, colors = colors)

  colors <- leaflet::colorBin(palette = colors,
                              bins = breaks,
                              na.color = "transparent")
  # pretty = TRUE)
    
  # # Then you can do:
  leaflet::addLegend(map = m,
                     pal = colors,
                     opacity = opacity,
                     values = breaks,
                     title = legendTitle)
  
}
