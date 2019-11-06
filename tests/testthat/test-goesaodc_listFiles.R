context("test-goesaodc_listFiles")

# ---- TEST PARAMETERS ---------------------------------------------------------
satID <- "G16"
datetime <- "2019-09-06 16"
endTime <- "2019-09-06 17"
timezone <- "America/Los_Angeles"
jdate <- "20192491600"
jdate_end <- "20192491700"

test_that("remote files are listed correctly", {
  
  expect_equal(
    length( 
      goesaodc_listFiles(
        satID = satID, 
        datetime = datetime, 
        timezone = timezone, 
        useRemote = TRUE)
    ),
    12
  )
  
})

test_that("remote files are listed correctly with Julian date", {
  
  expect_equal(
    length( 
      goesaodc_listFiles(
        satID = satID, 
        datetime = jdate, 
        timezone = timezone,
        isJulian = TRUE,
        useRemote = TRUE)
    ),
    12
  )
  
})
# ----- Download 2 hours of data -----------------------------------------------
# NOTE - This is bad practice, I should just "touch" a couple files in the data
# dir with the right strings in the filename as part of the setup and then
# delete them in teardown.

goesaodc_downloadAOD(satID = satID, 
                     datetime = datetime, 
                     endTime = endTime,
                     timezone = timezone)

# ---------------------------------------------------------------------------- #
test_that("local files are listed correctly", {
  
  expect_equal(
    length( 
      goesaodc_listFiles(
        satID = satID, 
        datetime = datetime,
        endTime = endTime,
        timezone = timezone)
    ),
    24
  )
  
})

test_that("local files are listed correctly using Julian format dates", {
  
  expect_equal(
    length( 
      goesaodc_listFiles(
        satID = satID, 
        datetime = jdate,
        endTime = jdate_end,
        timezone = timezone,
        isJulian = TRUE)
    ),
    24
  )
  
})
