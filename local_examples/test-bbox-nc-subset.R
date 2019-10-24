# Can we figure out bounding box indices?

library(MazamaSatelliteUtils)

setSatelliteDataDir("~/Data/Satellite")

ncFile <- system.file(
  "extdata",
  "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc",
  package = "MazamaSatelliteUtils"
)

nc <- goesaodc_openFile(ncFile)




# ----- Get grid data --------------------------------------------------------

# Get satID from netCDF, will be either "G16" or "G17"
satID <- ncdf4::ncatt_get(nc, varid = 0, attname = "platform_ID")$value

# Choose which gridFile to load based on satID
if ( satID == "G16") {
  gridFile <- "goesEastGrid.rda"
} else if ( satID == "G17" ) {
  gridFile <- "goesWestGrid.rda"
}

# Assemble the correct filepath based on satID and Data directory
filePath <- file.path(getSatelliteDataDir(), gridFile)

# Test for grid existence and if found, load it. Stop with appropriate message
# if missing
if ( file.exists(filePath) ) {
  goesGrid <- get(load(filePath))
} else {
  stop("Grid file not found. Run 'installGoesGrids()' first")
}  

fullLonMatrix <- goesGrid$longitude
fullLatMatrix <- goesGrid$latitude

bbox_oregon <- c(-124.56624, -116.46350, 41.99179, 46.29203)

lonLo <- bbox_oregon[1]
lonHi <- bbox_oregon[2]
latLo <- bbox_oregon[3]
latHi <- bbox_oregon[4]

# -----

lonMatrix <- fullLonMatrix[200:300,200:300]
latMatrix <- fullLatMatrix[200:300,200:300]

# An experiment:

mask <- lonMatrix > lonLo & latMatrix > latLo
image(mask)
apply(mask, 1, function(x) { min(which(x)) })
rowMinIndex <- apply(mask, 1, function(x) { min(which(x)) })
length(rowMinIndex)
rowMinIndex[rowMinIndex == Inf] <- 1e6
rowMinIndex
min(rowMinIndex)

