# Read and plot MAIAC data for the Sand fire
#
# https://inciweb.nwcg.gov/incident/4878/

library(maps)
library(MazamaSpatialUtils)

source('./R/loadRawMaiac.R')

# Read in example data
maiac <- loadRawMaiac("./localData/maiac_sands_AQUA_20160721.nc")

# dim(maiac)
# [1] 147961     3

# Speed up initial plots
indices <- seq(1,147961,20)
map('state','california')
# points(maiac$longitude[indices], maiac$latitude[indices], cex=0.5, pch=17, col='lightblue')
points(maiac$longitude, maiac$latitude, cex=0.5, pch=17, col='lightblue')

# Now colored by AOT
N <- 9
colors <- RColorBrewer::brewer.pal(N,'YlOrBr')
# Use classIntervals and try out different styles
breaks <- classInt::classIntervals(maiac$aot, n=9, style="quantile")$brks
colorIndex <- .bincode(maiac$aot, breaks, include.lowest=TRUE)

map('state','california')
# points(maiac$longitude[indices], maiac$latitude[indices], cex=0.5, pch=17, col=colors[colorIndex[indices]])
points(maiac$longitude, maiac$latitude, cex=0.5, pch=17, col=colors[colorIndex])

# Zoomed in
setSpatialDataDir('~/Data/Spatial')
loadSpatialData("USCensusCounties")
CA_counties <- subset(USCensusCounties, stateCode=="CA")
# dim(CA_counties@data)
# CA_counties@data
indices <- seq(1,147961,1)
plot(subset(CA_counties, name %in% c("Los Angeles","Santa Clarita","Ventura","Orange")), lwd=2)
points(maiac$longitude[indices], maiac$latitude[indices], cex=0.5, pch=17, col=colors[colorIndex[indices]])
plot(subset(CA_counties, name %in% c("Los Angeles","Santa Clarita","Ventura","Orange")), border=adjustcolor('antiquewhite',0.5), lwd=2, add=TRUE)


