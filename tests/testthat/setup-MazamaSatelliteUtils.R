# testthat unit test setup

library(MazamaSatelliteUtils)
library(MazamaSpatialUtils)

setSatelliteDataDir("~/Data/Satellite")
setSpatialDataDir("~/Data/Spatial")

goesaodc_installGoesGrids()
