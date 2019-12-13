context("test-goesaodc_createTibble")

# ---- TEST PARAMS -------------------------------------------------------------

goesaodc_downloadAOD(
  satID = "G16", 
  datetime = "201924918", 
  timezone = "UTC", 
  isJulian = TRUE)

ncFile <- "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc"

nc <- goesaodc_openFile(ncFile)
kincade_bbox <- c(-124, -120, 36, 39)

# ---- Original, full extent use case ------------------------------------------

test_that("Create tibble from full extents", {
  
  expect_error( 
    goesaodc_createTibble(nc),
    NA
  )
  
})

# ---- BBOX filtered use case --------------------------------------------------

test_that("Create tibble from filtered bbox extents", {
  
  expect_error(
    goesaodc_createTibble(nc, bbox = kincade_bbox),
    NA
  )
  
})

# --- Test that filtering is correct -------------------------------------------

test_that("tibble bbox filtering is correct", {
  
  filter_tbl <- goesaodc_createTibble(
    nc,
    bbox = kincade_bbox
  )
  
  min_lon <- min(filter_tbl$lon)
  max_lon <- max(filter_tbl$lon)
  min_lat <- min(filter_tbl$lat)
  max_lat <- max(filter_tbl$lat)
  
  # createTibble() should toss out anything outside the bbox
  expect_lte(kincade_bbox[1], min_lon)
  expect_gte(kincade_bbox[2], max_lon)
  expect_lte(kincade_bbox[3], min_lat)
  expect_gte(kincade_bbox[4], max_lat)
  
})
