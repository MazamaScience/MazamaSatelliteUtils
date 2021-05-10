context("test-goesaodc_listDaytimeFiles")

# ----- TEST PARAMETERS --------------------------------------------------------

satID <- "G16"
datetime <- "2019-09-06"
timezone <- "America/Los_Angeles"
jdate <- "2019249"

test_that("remote files are listed correctly for daytime", {
  
  skip_on_cran()
  skip_on_travis()
  
  expect_equal(
    length(
      goesaodc_listDaytimeFiles(
        satID = satID, 
        datetime = datetime, 
        timezone = timezone, 
        useRemote = TRUE
      )
    ),
    168
  )
  
})

test_that("remote files are listed correctly for daytime with Julian date", {
  
  skip_on_cran()
  skip_on_travis()
  
  expect_equal(
    length( 
      goesaodc_listDaytimeFiles(
        satID = satID, 
        datetime = jdate, 
        timezone = timezone,
        isJulian = TRUE,
        useRemote = TRUE)
    ),
    168
  )
  
})

# ----- Download data for the day-----------------------------------------------

test_that("local files are listed correctly for daytime", {
  
  skip_on_cran()
  skip_on_travis()
  
  goesaodc_downloadDaytimeAOD(
    satID = "G16", 
    datetime = "2019-09-06 16",
    timezone = timezone
  )
  
  expect_equal(
    length(
      goesaodc_listDaytimeFiles(
        satID = "G16", 
        datetime = "2019-09-06 16",
        timezone = "America/Los_Angeles", 
        useRemote = TRUE
      )
    ),
    168
  )
  
})
