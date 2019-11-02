context("test-goesaodc_convertFilenameToDatetime")

test_that("correct datetime string is returned", {
  
  expect_equal(
    goesaodc_convertFilenameToDatetime(
      "OR_ABI-L2-AODC-M6_G16_s20192491601094_e20192491603467_c20192491610121.nc"
    ), MazamaCoreUtils::parseDatetime("2019-09-06 16:01:09", "UTC")
  )
}
)
