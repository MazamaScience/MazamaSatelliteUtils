context("test-goesaodc_downloadaod")

test_that("data can be downloaded", {
  
  # save old satelliteDataDir if it exists
  olddir <- NULL
  try({
    olddir <- getSatelliteDataDir()
  }, silent = TRUE)
  
  # set satelliteDataDir to be the tempdir
  tempdir <- tempdir()
  setSatelliteDataDir(tempdir)
  
  # download files for Noon, May 16th 2019 UTC
  startdate <- 2019051612
  downloaded_files <- goesaodc_downloadAOD("G16", startdate)
    
  # should have downloaded 12 files
  expect_length(downloaded_files, 12)
  
  # get rid of downloaded files
  file.remove(paste0(getSatelliteDataDir(), "/", downloaded_files))
  
  # reset satelliteDataDir
  if (!is.null(olddir)) {
    setSatelliteDataDir(olddir)
  }
})
