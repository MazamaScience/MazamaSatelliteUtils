context("test-goesaodc_downloadDaytimeAOD")

# ---- TEST PARAMETERS ---------------------------------------------------------
satID <- "G16"
datetime <- "2019-09-06"
timezone <- "America/Los_Angeles"
jdate <- "2019249"

# ---- TESTS -------------------------------------------------------------------
test_that("Basic daytime download works", {
  
  expect_error(
    goesaodc_downloadDaytimeAOD(satID = satID, 
                                datetime = datetime, 
                                timezone = timezone),
    NA)
  
})

test_that("Julian date daytime download works", {
  
  expect_error(
    goesaodc_downloadDaytimeAOD(satID = satID, 
                                datetime = jdate, 
                                timezone = timezone,
                                isJulian = TRUE),
    NA)
  
})
