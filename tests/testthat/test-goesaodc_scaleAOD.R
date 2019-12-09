context("test-goesaodc_scaleAOD")

# ---- Setup the date needed for function to run -------------------------------
aod_test_nc <- ncdf4::nc_open("aod_test.nc")

# Copied from goesaodc_createNativeGrid()
test_aod_data <- ncdf4::ncvar_get(
  aod_test_nc,
  varid = "AOD",
  verbose = FALSE,
  signedbyte = FALSE,
  collapse_degen = TRUE,
  raw_datavals = TRUE
)

aod_metadata <- ncdf4::ncatt_get(aod_test_nc, "AOD")

# Create list of metadata needed for conversion
conversion_factors <- list()
conversion_factors$fill_value <- aod_metadata$'_FillValue'
conversion_factors$aod_scale_factor <- aod_metadata$scale_factor
conversion_factors$aod_offset <- aod_metadata$add_offset

# ---- Test that function runs without generating errors -----------------------
test_that("Extract and scale AOD values from .nc file", {
  
  expect_error( goesaodc_scaleAOD(test_aod_data, conversion_factors),
                NA)
})

# ---- Test that scaled AOD values are correct ---------------------------------
test_that("Check that AOD scaling and NA setting are correct", {
  # Raw AOD values going into function
  # -1 -30111   3290  11230     -3
  
  # Should return
  # NA 2.6798506 0.2035274 0.8153838 4.9999731
  
  # Test for expected values
  scaled_aod_values <- goesaodc_scaleAOD(test_aod_data, conversion_factors)
  expect_true(is.na(scaled_aod_values[1]))   
  expect_true(round(scaled_aod_values[2], 3) == 2.68)
  expect_true(round(scaled_aod_values[3], 2) == 0.2)
  expect_true(round(scaled_aod_values[4], 2) == 0.82)
  expect_true(round(scaled_aod_values[5], 2) == 5.0)
  
  
})