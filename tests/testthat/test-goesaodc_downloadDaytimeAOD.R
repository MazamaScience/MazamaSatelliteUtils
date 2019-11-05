context("test-goesaodc_downloadDaytimeAOD")

test_that("files are downloaded for daytime", {
  
  expect_error(
    goesaodc_downloadDaytimeAOD(satID = "G16", 
                                datetime = "2019-09-06", 
                                timezone = "America/Los_Angeles"),
    NA)
  
})
