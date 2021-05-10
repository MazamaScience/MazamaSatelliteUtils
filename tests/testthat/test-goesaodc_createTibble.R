context("test-goesaodc_createTibble")

# ---- TEST PARAMS -------------------------------------------------------------

scanFile <- goesaodc_downloadScanFiles(
  satID = "G17", 
  datetime = "2020-09-09 00:00", 
  timezone = "UTC"
)

nc <- goesaodc_openFile(scanFile)
kincade_bbox <- c(-124, -120, 36, 39)

# ---- Original, full extent use case ------------------------------------------

test_that("create tibble from full extents", {
  
  expect_error( 
    goesaodc_createTibble(nc),
    NA
  )
  
})

# ---- BBOX filtered use case --------------------------------------------------

test_that("create tibble from filtered bbox extents", {
  
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
