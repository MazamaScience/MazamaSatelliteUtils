# GOESAOD documentation: https://data.nodc.noaa.gov/cgi-bin/iso?id=gov.noaa.ncdc:C01511
# https://tools-1.airfire.org/Satellite/GOES-16/AODC/


# exerpt from print(nc):
# 
# float geospatial_lat_lon_extent[]   (Contiguous storage)  
#   long_name: geospatial latitude and longitude references
#   geospatial_westbound_longitude: -152.109283447266
#   geospatial_northbound_latitude: 56.7614517211914
#   geospatial_eastbound_longitude: -52.9468803405762
#   geospatial_southbound_latitude: 14.5713396072388
#   geospatial_lat_center: 30.0830020904541
#   geospatial_lon_center: -87.0969543457031
#   geospatial_lat_nadir: 0
#   geospatial_lon_nadir: -75
#   geospatial_lat_units: degrees_north
#   geospatial_lon_units: degrees_east


library(ncdf4)
library(raster)

# load .nc file
file_path <- file.path('../local_data','OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc')
nc <- ncdf4::nc_open(file_path)

# get geospatial lat lon extent info
lat_lon_extent <- ncatt_get(nc, "geospatial_lat_lon_extent")
ymn <- lat_lon_extent$geospatial_southbound_latitude
ymx <- lat_lon_extent$geospatial_northbound_latitude
xmn <- lat_lon_extent$geospatial_westbound_longitude
xmx <- lat_lon_extent$geospatial_eastbound_longitude

# create AOD raster
aod <- ncvar_get(nc, "AOD")
aod_raster <- raster(t(aod), xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)

# goesaod_downloadData <- function(
#   date = NULL,
#   time = NULL,
#   julianDate = NULL,
#   baseUrl
# )
