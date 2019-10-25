context("test-goesaodc_getdaylighthours")

test_that("Correct daylight sunrise and sunset hours are calculated", {
  
  # ---- TEST TIMES BASED ON LAT/LON AND DATE ----------------------------------
  # TEST_DATA
  sunrise_ll <- MazamaCoreUtils::parseDatetime("2019-09-06 06:42:52", 
                                               timezone = "America/Los_Angeles")
  sunset_ll <- MazamaCoreUtils::parseDatetime("2019-09-06 19:39:02", 
                                              timezone = "America/Los_Angeles")
  ll_dayInfo <- list("sunrise" = sunrise_ll, "sunset" = sunset_ll)
  
  # GET DAYLIGHT HOURS WITH LAT/LON
  ll_test_data <- goesaodc_getDaylightHours(datetime = "2019-09-06", 
                                            longitude = -123.245, 
                                            latitude = 42.861)
  # CALCULATE TIME DIFF
  ll_sunrise_diff <- as.numeric(difftime(ll_dayInfo$sunrise,
                                         ll_test_data$sunrise,
                                         units = "secs"))
                                         
                                
  ll_sunset_diff <- as.numeric(difftime(ll_dayInfo$sunset, 
                                        ll_test_data$sunset, 
                                        units = "secs"))
  # ASSERTION
  expect_lt(ll_sunrise_diff, 1)
  expect_lt(ll_sunset_diff, 1)
})
