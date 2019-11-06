context("test-goesaodc_downloadaod")

# ---- TEST PARAMS -------------------------------------------------------------
satID <- "G16"
datetime <- "2019-09-06 09"
endTime <- "2019-09-06 10"
timezone <- "America/Los_Angeles"
jdate <- "2019249090000"
jdate_end <- "2019249100000"

# ---- FAIL AS EXPECTED --------------------------------------------------------
test_that("fails when passed incorrect parameters", {
  
  # invalid date format: YYMMDDHH
  expect_error(
    goesaodc_downloadAOD(datetime = "19033112",
                         satID = satID) ,
    regexp = "No datetimes could be parsed."
  )
  
  # no startdate passed
  expect_error(
    goesaodc_downloadAOD(satID = satID),
    regexp = "argument 'datetime' must not be NULL."
  )
  
  # no satID given
  expect_error(
    goesaodc_downloadAOD(datetime = datetime),
    regexp = "argument 'satID' must not be NULL."
  )
  
  # More than 24 hours of data requested
  expect_error(
    goesaodc_downloadAOD(satID = satID,
                         datetime = "2019-09-06",
                         endTime = "2019-09-08"),
    regexp = "More than 24 hours of data requested."
  )
  
})

# ---- TESTS THAT SHOULD PASS --------------------------------------------------

test_that("Basic file download works", {
  
  expect_error( goesaodc_downloadAOD(
    satID = satID,
    datetime = datetime,
    endTime = endTime,
    timezone = timezone),
    NA)
})

test_that("File download using Julian date format", {
  
  expect_error( goesaodc_downloadAOD(
    satID = satID,
    datetime = jdate,
    endTime = jdate_end,
    timezone = timezone,
    isJulian = TRUE),
    NA)
})
