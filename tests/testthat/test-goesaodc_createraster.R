context("test-goesaodc_createraster")

# ---- TO DO: Get this block in a generic setup file ---------------------------
library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")
bbox_or <- c(-124.56624, -116.46350, 41.99179, 46.29203) # Oregon

goesaodc_downloadAOD(
  satID = "G16", 
  datetime = "201924918", 
  timezone = "UTC", 
  isJulian = TRUE)

netCDF <- "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc"

nc <- goesaodc_openFile(netCDF)
# ----- END TO DO --------------------------------------------------------------

test_that("function runs", {
   expect_error(goesaodc_createRaster(nc, 
                                      res = 0.1, 
                                      dqfLevel = 2,
                                      bbox = bbox_or), NA)
}

)  # END OF TEST
