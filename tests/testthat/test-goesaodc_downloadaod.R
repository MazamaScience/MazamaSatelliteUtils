context("test-goesaodc_downloadaod")

test_that("fails on when passed incorrect parameters", {
  
  # date of format: YYMMDDHH
  expect_error({
    startdate <- "19033112"
    goesaodc_downloadAOD(startdate)
  })
  
  # no startdate passed
  expect_error({
    goesaodc_downloadAOD()
  })
})
