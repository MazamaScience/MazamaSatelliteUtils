context("test-goesaodc_createRasterStack")

# ---- Create basic parameters for raster stacking -----------------------------
satID <- "G16"
datetime <- "2019-09-06 09:00"
endTime <- "2019-09-06 10:00"
bbox <- c(-124.56624, -116.46350, 41.99179, 46.29203) # Oregon
dqfLevel <- 2
timezone <- "America/Los_Angeles"

# ---- Test basic raster stacking with a start and end time --------------------

test_that("Basic raster stacking works", {
  
  expect_error( goesaodc_createRasterStack(
    satID = satID,
    datetime = datetime,
    endTime = endTime,
    bbox = bbox,
    dqfLevel = dqfLevel,
    timezone = timezone),
    NA)
})

# ---- Test that stacking from a fileList works --------------------------------

test_that("Raster stacking from a fileList works", {
  
  fileList <- goesaodc_listFiles(satID = satID,
                                 datetime = datetime,
                                 endTime = endTime,
                                 timezone = timezone)

  expect_error( goesaodc_createRasterStack(
    satID = satID,
    datetime = datetime,
    endTime = endTime,
    bbox = bbox,
    dqfLevel = dqfLevel,
    timezone = timezone,
    fileList = fileList),
    NA)
})
