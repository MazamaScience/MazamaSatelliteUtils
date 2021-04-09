context("test-goesaodc_scaleAOD")

# ---- Test that scaled AOD values are correct ---------------------------------

test_that("AOD scaling and NA setting are correct", {
  
  # Same attributes found in GOES AOD netcdf files
  aod_attributes <- list(
    "_FillValue"          = -1,
    "long_name"           = "ABI L2+ Aerosol Optical Depth at 550 nm",
    "standard_name"       = "atmosphere_extinction_optical_thickness_due_to_ambient_aerosol",
    "_Unsigned"           = "true",
    "valid_range"         = c(0, -6),
    "scale_factor"        = 7.71e-05,
    "add_offset"          = -0.05,
    "units"               = "1",
    "resolution"          = "y: 0.000056 rad x: 0.000056 rad",
    "coordinates"         = "sunglint_angle retrieval_local_zenith_angle quantitative_local_zenith_angle retrieval_solar_zenith_angle quanti ...",
    "grid_mapping"        = "goes_imager_projection",
    "cell_methods"        = "sunglint_angle: point (no pixel produced over sea only) retrieval_local_zenith_angle: point (good or degraded q ...",
    "ancillary_variables" = "DQF"
  )
  
  # NOTE:  aod_raw is interpreted as signed bytes so 11111111 11111100 is:
  # NOTE:     (-1*2^15 + 1*2^14 + ... + 0*2^1 + 0*2^0) = -3
  # NOTE:  The conversion function interprets these bits as an unsigned byte:
  # NOTE:     ( 1*2^15 + 1*2^14 + ... + 0*2^1 + 0*2^0) = 65533
  # NOTE:  The conversion function also applies scale and offset:
  # NOTE:                      65533 * 7.71e-05 - 0.05 = 5.002594

  aod_raw <- c(-1, -30111, 3290, 11230, -3)
  aod_converted <- c(NA, 2.681268, 0.203659, 0.815833, 5.002594)
  
  expect_equal(
    goesaodc_scaleAOD(aod_raw, aod_attributes),
    aod_converted,
    tolerance = 1/2^15
  )

})