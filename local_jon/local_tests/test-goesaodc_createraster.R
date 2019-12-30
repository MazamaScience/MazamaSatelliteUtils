context("goesaodc_createraster")

test_that("extent and dimention of rasters are consistant", {
  
  oldDir <- NULL
  try({
    oldDir <- getSatelliteDataDir()
  }, silent = TRUE)
  
  setSatelliteDataDir("local_data/CampFire/001")
  
  startdate <- 2018111515
  
  # Use bbox for California to speed up test
  bb_ca <- sp::bbox(matrix(c(-124.4096, 32.53416, -114.1312, 42.00952), 
                           ncol = 2, byrow = TRUE))
  
  # create a list of rasters
  rasterList <-
    goesaodc_listFiles("G16", startdate) %>%
    purrr::map(goesaodc_openFile) %>%
    purrr::map(goesaodc_createRaster, bbox = bb_ca)
  
  # test that extent is the same
  for(i in c(2:12)) {
    testthat::expect_equal(raster::extent(rasterList[[1]]), 
                           raster::extent(rasterList[[i]]))
  }
  
  # test that dimentions are the same
  for(i in c(2:12)) {
    testthat::expect_equal(dim(rasterList[[1]]$AOD), 
                           dim(rasterList[[i]]$AOD))
  }
  
  # reset satelliteDataDir
  if (!is.null(oldDir)) {
    setSatelliteDataDir(oldDir)
  }
  
})
