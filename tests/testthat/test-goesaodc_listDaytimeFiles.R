context("test-goesaodc_listDaytimeFiles")

# ---- TEST PARAMETERS ---------------------------------------------------------
satID <- "G16"
datetime <- "2019-09-06"
timezone <- "America/Los_Angeles"
jdate <- "2019249"

test_that("remote files are listed correctly for daytime", {
  
  expect_equal(
    length( 
      goesaodc_listDaytimeFiles(
        satID = satID, 
        datetime = datetime, 
        timezone = timezone, 
        useRemote = TRUE)
    ),
    168
  )
  
})

test_that("remote files are listed correctly for daytime with Julian date", {
  
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
# ----- Download 2 hours of data -----------------------------------------------
# NOTE - This is bad practice, I should just "touch" a couple files in the data
# dir with the right strings in the filename as part of the setup and then
# delete them in teardown.

goesaodc_downloadAOD(satID = "G16", 
                     datetime = "2019-08-12 16", 
                     endTime = "2019-08-12 17")

# ---------------------------------------------------------------------------- #
test_that("local files are listed correctly for daytime", {
  
  expect_equal(
    length( 
      goesaodc_listDaytimeFiles(
        satID="G16", 
        datetime="2019-09-06", 
        timezone="America/Los_Angeles", 
        useRemote=FALSE)),
    24
  )
  
})
