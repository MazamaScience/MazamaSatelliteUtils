################################################################################
#
# TEMPORARY DISABLED
#
# Currently takes 225 seconds to run
#
################################################################################


# context("test-goesaodc_createRasterStack")
# 
# # ---- Create basic parameters for raster stacking -----------------------------
# satID <- "G16"
# datetime <- "2019-09-06 09:00"
# endtime <- "2019-09-06 10:00"
# bbox <- c(-124.56624, -116.46350, 41.99179, 46.29203) # Oregon
# dqfLevel <- 2
# timezone <- "America/Los_Angeles"
# 
# # ---- Parameters for Julian Date tests ----------------------------------------
# jdate <- "20192490900"
# jdate_end <- "20192491000"
# 
# # ---- Test basic raster stacking with a start and end time --------------------
# 
# test_that("Basic raster stacking works", {
#   
#   expect_error( goesaodc_createRasterStack(
#     satID = satID,
#     datetime = datetime,
#     endtime = endtime,
#     bbox = bbox,
#     dqfLevel = dqfLevel,
#     timezone = timezone),
#     NA)
# })
# 
# # ---- Test raster stacking using Julian formatted datetimes -------------------
# 
# test_that("Raster stacking using Julian datetimes work", {
#   expect_error( goesaodc_createRasterStack(
#     satID = satID,
#     datetime = jdate,
#     endtime = jdate_end,
#     isJulian = TRUE,
#     bbox = bbox,
#     dqfLevel = dqfLevel,
#     timezone = timezone),
#     NA)
# })
# 
# # ---- Test that stacking from a fileList works --------------------------------
# 
# test_that("Raster stacking from a fileList works", {
#   
#   fileList <- goesaodc_listFiles(satID = satID,
#                                  datetime = datetime,
#                                  endtime = endtime,
#                                  timezone = timezone)
# 
#   expect_error( goesaodc_createRasterStack(
#     satID = satID,
#     datetime = datetime,
#     endtime = endtime,
#     bbox = bbox,
#     dqfLevel = dqfLevel,
#     timezone = timezone,
#     fileList = fileList),
#     NA)
# })
