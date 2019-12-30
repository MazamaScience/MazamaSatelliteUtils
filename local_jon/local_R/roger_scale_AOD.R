# I Really don't want to add yet more crap inside the existing function. And I'd like to have the
# conversion and scaling take place together

library(ncdf4)

scaleAOD <- function (
    
aod_data,
conversion_factors

) {
    
    # Convert fill_values to NA
    fill_values <- aod_data == conversion_factors$fill_value
    aod_data[fill_values] <- NA
    negative_values <- aod_data < 0
    
    # Convert the negative values to proper unsigned short values
    negative_values <- which(aod_data < 0)
    aod_data[negative_values] <- aod_data[negative_values] + 65536
    
    # Scale AOD data into proper range
    aod_scale_factor <- conversion_factors$aod_scale_factor
    aod_offset <- conversion_factors$aod_offset

    scaled_aod <- (aod_data * aod_scale_factor) + aod_offset
    
    return(scaled_aod)
}


aod_test_nc <- ncdf4::nc_open("aod_test.nc")
test_aod_data <- ncdf4::ncvar_get(
      aod_test_nc,
      varid = "AOD",
      verbose = FALSE,
      signedbyte = FALSE,
      collapse_degen = TRUE,
      raw_datavals = TRUE
    )

aod_metadata <- ncatt_get(aod_test_nc, "AOD")

conversion_factors <- list()
conversion_factors$fill_value <- aod_metadata$'_FillValue'
conversion_factors$aod_scale_factor <- aod_metadata$scale_factor
conversion_factors$aod_offset <- aod_metadata$add_offset

scaleAOD(test_aod_data, conversion_factors)