library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)

setSatelliteDataDir("~/Data/Satellite")
setSpatialDataDir("~/Data/Spatial")

loadSpatialData("NaturalEarthAdm1")

bbox <- c(-105, -89, 42, 49.5)

goesaodc_downloadScanFiles(
  satID = "G16",
  datetime = "2021-05-12 08:00",
  endtime = "2021-05-12 18:00",
  timezone = "America/Chicago",
  verbose = TRUE
)

# -----

scanFile <- goesaodc_listScanFiles(
  satID = "G16",
  datetime = "2021-05-12 09:00",
  timezone = "America/Chicago"
)

scanRaster <- goesaodc_createScanRaster(
  filename = scanFile,
  bbox = bbox,
  cellSize = 0.05
)

goesaodc_plotScanRaster(
  raster = scanRaster,
  bbox = bbox,
  #paletteBreaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
  #includeMap = TRUE,
  #zoom = 5,
  stateCodes = c("ND", "SD", "MN")
)

# ----- Generate video

./goesaodc_animateScanRasters_exec.R \
--satID="G16" \
--starttime="2021-05-12 09:00" \
--endtime="2021-05-12 18:00" \
--timezone="America/Chicago" \
--bbox="-105, -89, 42, 49.5" \
--dqfLevel=3 \
--cellSize=0.04 \
--legendLimits="-0.5, 5.5" \
--stateCodes="ND, SD, MN" \
--satelliteDataDir="~/Data/Satellite" \
--spatialDataDir="~/Data/Spatial" \
--frameRate=6 \
--outputDir="~/Desktop" \
--logDir="~/Desktop" \
--verbose=TRUE
