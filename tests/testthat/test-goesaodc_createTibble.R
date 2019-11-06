context("test-goesaodc_createTibble")

# ---- TEST PARAMS -------------------------------------------------------------
ncFile <- system.file(
   "extdata", 
   "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc", 
   package = "MazamaSatelliteUtils")

nc <- goesaodc_openFile(ncFile)
kincade_bbox <- c(-124, -120, 36, 39)

# ---- Original, full extent use case ------------------------------------------
test_that("Create tibble from full extents", {
  
  expect_error( goesaodc_createTibble(nc),
    NA)
})

# ---- BBOX filtered use case --------------------------------------------------
test_that("Create tibble from filtered bbox extents", {
  
  expect_error( goesaodc_createTibble(
    nc,
    bbox = kincade_bbox),
    NA)
})

# --- Test that filtering is correct -------------------------------------------
test_that("tibble bbox filtering is correct", {
  
  filter_tbl <- goesaodc_createTibble(
    nc,
    bbox = kincade_bbox)
  
  min_lon <- min(filter_tbl$lon)
  max_lon <- max(filter_tbl$lon)
  min_lat <- min(filter_tbl$lat)
  max_lat <- max(filter_tbl$lat)
  
  expect_lt(min_lon, kincade_bbox[1])
  expect_gt(max_lon, kincade_bbox[2])
  expect_lt(min_lat, kincade_bbox[3])
  expect_gt(max_lat, kincade_bbox[4])

})
