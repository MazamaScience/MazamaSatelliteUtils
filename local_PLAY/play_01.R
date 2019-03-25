# Explore GOES_AOD data
#
# Excerpted from ncdump -h ...:
#
# dimensions:
# 	y = 1500 ;
# 	x = 2500 ;
# 	number_of_time_bounds = 2 ;
# 	band = 1 ;
# 	number_of_image_bounds = 2 ;
# 	number_of_sunglint_angle_bounds = 2 ;
# 	number_of_LZA_bounds = 2 ;
# 	number_of_SZA_bounds = 2 ;
# 	land_sensor_bands = 3 ;
# 	sea_sensor_bands = 4 ;
# 	latitude_bands = 18 ;
# 	number_of_latitude_band_bounds = 2 ;
# 	num_area_types = 2 ;
# 	max_area_type_len = 14 ;
# variables:
# 	short AOD(y, x) ;
# 		AOD:_FillValue = -1s ;
# 		AOD:long_name = "ABI L2+ Aerosol Optical Depth at 550 nm" ;
# 		AOD:standard_name = "atmosphere_extinction_optical_thickness_due_to_ambient_aerosol" ;
# 		AOD:_Unsigned = "true" ;
# 		AOD:valid_range = 0s, -6s ;
# 		AOD:scale_factor = 7.706e-05f ;
# 		AOD:add_offset = -0.05f ;
# 		AOD:units = "1" ;
# 		AOD:resolution = "y: 0.000056 rad x: 0.000056 rad" ;
# 		AOD:coordinates = "sunglint_angle retrieval_local_zenith_angle quantitative_local_zenith_angle retrieval_solar_zenith_angle quantitative_solar_zenith_angle aod_product_wavelength t y x" ;
# 		AOD:grid_mapping = "goes_imager_projection" ;
# 		AOD:cell_methods = "sunglint_angle: point (no pixel produced over sea only) retrieval_local_zenith_angle: point (good or degraded quality pixel produced) quantitative_local_zenith_angle: point (good quality pixel produced) retrieval_solar_zenith_angle: point (good or degraded quality pixel produced) quantitative_solar_zenith_angle: point (good quality pixel produced) t: point area: point" ;
# 		AOD:ancillary_variables = "DQF" ;
# ...
# 	short y(y) ;
# 		y:scale_factor = -5.6e-05f ;
# 		y:add_offset = 0.128212f ;
# 		y:units = "rad" ;
# 		y:axis = "Y" ;
# 		y:long_name = "GOES fixed grid projection y-coordinate" ;
# 		y:standard_name = "projection_y_coordinate" ;
# 	short x(x) ;
# 		x:scale_factor = 5.6e-05f ;
# 		x:add_offset = -0.101332f ;
# 		x:units = "rad" ;
# 		x:axis = "X" ;
# 		x:long_name = "GOES fixed grid projection x-coordinate" ;
# 		x:standard_name = "projection_x_coordinate" ;


library(ncdf4)

filePath <- file.path('../data','OR_ABI-L2-AODC-M3_G16_s20190580907135_e20190580909508_c20190580910238.nc')
nc <- ncdf4::nc_open(filePath)

# x(x), y(y)
x <- as.numeric(ncdf4::ncvar_get(nc, "x"))
y <- as.numeric(ncdf4::ncvar_get(nc, "y"))
aod_unraveled <- as.numeric(ncdf4::ncvar_get(nc, "AOD"))

# Not looking so good:
#
# summary(aod_unraveled)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#      NA      NA      NA     NaN      NA      NA 3750000 


# AOD is aod(y,x)
# First stab at reassembling
aod <- matrix(data = aod_unraveled,
              nrow = length(x),
              ncol = length(y),
              byrow = FALSE)


              


