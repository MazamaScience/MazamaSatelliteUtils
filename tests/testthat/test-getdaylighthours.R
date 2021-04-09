context("test-getdaylighthours")

test_that("correct daylight sunrise and sunset hours are calculated", {
  
  library(MazamaSpatialUtils)
  
  # ----- TEST_SETUP -----------------------------------------------------------
  
  sunrise_ll <- MazamaCoreUtils::parseDatetime(
    "2019-09-06 06:42:52", 
    timezone = "America/Los_Angeles"
  )
  sunset_ll <- MazamaCoreUtils::parseDatetime(
    "2019-09-06 19:39:02", 
    timezone = "America/Los_Angeles"
  )
  ll_dayInfo <- list("sunrise" = sunrise_ll, "sunset" = sunset_ll)
  
  posix_t <- MazamaCoreUtils::parseDatetime(
    "2019-09-06 16", 
    "America/Los_Angeles"
  )
  
  # ----- GET DAYLIGHT HOURS WITH LAT/LON --------------------------------------
  ll_test_data <- getDaylightHours(
    datetime = "2019-09-06", 
    longitude = -123.245, 
    latitude = 42.861
  )
  
  # ----- GET DAYLIGHT HOURS WITH BBOX -----------------------------------------
  bb_test_data <- getDaylightHours(
    datetime = "2019-09-06", 
    bbox = c(-124.566, -116.463, 41.991, 46.292)
  )
  
  # ----- GET DAYLIGHT HOURS WITH DATETIME AND TIMEZONE ------------------------
  tz_test_data <- getDaylightHours(
    datetime = "2019-09-06", 
    timezone = "America/Los_Angeles"
  )
  
  # ----- GET DAYLIGHT HOURS WITH JULIAN FORMAT DATETIME AND TIMEZONE ----------
  
  julian_test_data <- getDaylightHours(
    datetime = "2019249",
    timezone = "America/Los_Angeles",
    isJulian = TRUE
  )
  
  # ----- GET DAYLIGHT HOURS WITH POSIXt ALONE ---------------------------------
  
  posix_test_data <- getDaylightHours(posix_t)
  
  # ----- LAT/LON TIME DIFFS ---------------------------------------------------
  
  ll_sunrise_diff <- as.numeric(difftime(
    ll_dayInfo$sunrise,
    ll_test_data$sunrise,
    units = "mins")
  )
  
  ll_sunset_diff <- as.numeric(difftime(
    ll_dayInfo$sunset, 
    ll_test_data$sunset, 
    units = "mins")
  )
  
  # ----- BBOX TIME DIFFS ------------------------------------------------------
  
  bb_sunrise_diff <- as.numeric(difftime(
    ll_dayInfo$sunrise,
    ll_test_data$sunrise,
    units = "mins")
  )
  
  bb_sunset_diff <- as.numeric(difftime(
    ll_dayInfo$sunset,
    ll_test_data$sunset,
    units = "mins"
  ))
  
  # ----- TZ TIME DIFFS --------------------------------------------------------
  
  tz_sunrise_diff <- as.numeric(difftime(
    ll_dayInfo$sunrise,
    tz_test_data$sunrise,
    units = "mins"
  ))
  
  tz_sunset_diff <- as.numeric(difftime(
    ll_dayInfo$sunset,
    tz_test_data$sunset,
    units = "mins"
  ))
  
  # ----- JULIAN TIME DIFFS ----------------------------------------------------
  
  
  julian_sunrise_diff <- as.numeric(difftime(
    ll_dayInfo$sunrise,
    julian_test_data$sunrise,
    units = "mins"
  ))
  
  julian_sunset_diff <- as.numeric(difftime(
    ll_dayInfo$sunset,
    julian_test_data$sunset,
    units = "mins"
  ))
  
  # ----- POSIX TIME DIFF (SHOULD BE EQUAL TO TZ TIME --------------------------
  
  posix_sunrise_diff <- as.numeric(difftime(
    tz_test_data$sunrise,
    posix_test_data$sunrise,
    units = "secs"
  ))
  
  posix_sunset_diff <- as.numeric(difftime(
    tz_test_data$sunset,
    posix_test_data$sunset,
    units = "secs"
  ))
  
  # ----- LAT/LON ASSERTIONS ---------------------------------------------------
  
  expect_lt(ll_sunrise_diff, 60)
  expect_lt(ll_sunset_diff, 60)
  
  # ----- BBOX ASSERTIONS ------------------------------------------------------
  
  expect_lt(bb_sunrise_diff, 60)
  expect_lt(bb_sunset_diff, 60)
  
  # ----- TIMEZONE ASSERTIONS --------------------------------------------------
  
  expect_lt(tz_sunrise_diff, 60)
  expect_lt(tz_sunset_diff, 60)
  
  # ----- JULIAN DATE ASSERTIONS -----------------------------------------------
  
  expect_lt(julian_sunrise_diff, 60)
  expect_lt(julian_sunset_diff, 60)
  
  # ----- POSIXt ASSERTIONS ----------------------------------------------------
  
  expect_equal(posix_sunrise_diff, 0)
  expect_equal(posix_sunset_diff, 0)
  
})

test_that("function fails when passed incorrect parameters", {
  
  # No datetime given
  expect_error(
    getDaylightHours(),
    regexp = "argument 'datetime' must not be NULL."
  )
  
  # No data available to derive timezone
  expect_error(
    getDaylightHours(datetime = "2019-09-06")
  )
  
})

test_that("function is vectorized", {
  
  daylight <- getDaylightHours(
    datetime = c("2019-06-21", "2019-09-23", "2019-12-22"),
    longitude = -123.245, 
    latitude = 42.861
  )
  
  expect_true(lubridate::is.POSIXt(daylight$sunrise))
  expect_equal(length(daylight$sunrise), 3)
  expect_equal(strftime(daylight$sunrise, "%Z"), c("PDT", "PDT", "PST"))
  
})