library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)
library(sp)
library(dismo)
library(rgdal)
library(gstat)
library(sf)
library(USAboundaries)

setSatelliteDataDir("~/Data/Satellite")
setSpatialDataDir("~/Data/Spatial")
#USAboundaries::install_data_package()

tx_simple <- USAboundaries::us_boundaries(type = "state", resolution = "low", states = c("Texas"))
plot(sf::st_geometry(tx_simple))

# get bbox for Texas
loadSpatialData("USCensusStates")
tx <- subset(USCensusStates, stateCode == "TX")
bbox_tx <- sp::bbox(tx)

nc <- goesaodc_openFile("OR_ABI-L2-AODC-M6_G16_s20191291201274_e20191291204047_c20191291210009.nc")
sp <- goesaodc_createSpatialPoints(nc, lonLo = -107, lonHi = -93, latLo = 30, latHi = 37)
sp::proj4string(sp) <- "+proj=longlat +datum=NAD83"

polys <- list("sp.polygons", tx, fill = "lightgray")
sp::spplot(sp, "AOD", sp.layout = polys, cex = 0.4)

# Transform points and Texas object to proper state projection
TA <- CRS(dplyr::filter(USAboundaries::state_proj, state == "TX", statewide_proj)[1,]$proj4_string)
dta <- spTransform(sp, TA)
taproj_tx <- spTransform(tx, TA)

# Generate Voronoi and clip it to the projected state boundary
v <- dismo::voronoi(dta)
agg_tx <- aggregate(taproj_tx)
voronoi_tx <- intersect(v, agg_tx)
pal_aod <- colorRampPalette(c("lightgoldenrod1", "red3"))
spplot(voronoi_tx, "AOD", regions = pal_aod(100), lty = "blank")

# Rasterize the Voronoi ?
r <- raster(taproj_tx)
vr <- rasterize(voronoi_tx, r, "AOD")
plot(vr)


