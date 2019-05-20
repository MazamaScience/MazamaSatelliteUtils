context("goesaodc_createraster")

test_that("extent and dimention of rasters are consistant", {
  
  olddir <- NULL
  try({
    olddir <- getSatelliteDataDir()
  }, silent = TRUE)
  
  setSatelliteDataDir("../local_data/CampFire/001")
  
  startdate <- 2018111515
  
  # Use bbox for California to speed up test
  bb_ca <- sp::bbox(matrix(c(-124.4096, 32.53416, -114.1312, 42.00952), 
                           ncol = 2, byrow = TRUE))
  
  # create a list of rasters
  rstList <-
    goesaodc_listFiles(startdate) %>%
    purrr::map(goesaodc_openFile) %>%
    purrr::map(goesaodc_createRaster, bbox = bb_ca)
  
  # test that extent is the same
  for(i in c(2:12)) {
    testthat::expect_equal(raster::extent(rstList[[1]]), 
                           raster::extent(rstList[[i]]))
  }
  
  # test that dimentions are the same
  for(i in c(2:12)) {
    testthat::expect_equal(dim(rstList[[1]]$AOD), 
                           dim(rstList[[i]]$AOD))
  }
  
  # reset satelliteDataDir
  if (!is.null(olddir)) {
    setSatelliteDataDir(olddir)
  }
  
})
