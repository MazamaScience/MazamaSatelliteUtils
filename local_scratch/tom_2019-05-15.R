# Tom's scratch
# 5/15/19

# ----- goesaodc_listFiles() bug Minimal Reproducable Example ------------------

setSatelliteDataDir("~/Data/Satellite")

# Download data for a desired date and hour
date <- 20190331
hour <- 16
datetime <- lubridate::parse_date_time(paste0(date, hour), "YmdH")

# This will check if the data already exists in satelliteDataDir. If not, it will
# download the data and if so it will do nothing. Either way, the data is in
# the satelliteDataDir once the line is finished running.
goesaodc_downloadAOD(date = date, hour = hour)

# Now try and list the files.
goesaodc_listFiles(datetime)

# This returns an empty character vector. At this point, the bug could either be
# in goesaodc_downloadAOD() or goesaodc_listFiles(). But, if we check the
# satelliteDataDir without using the function, we see that the files are there.

getSatelliteDataDir() %>%
  list.files() %>%
  stringr::str_subset(format(datetime, "%Y%j"))

# [1] "OR_ABI-L2-AODC-M3_G16_s20190901602158_e20190901604530_c20190901607078.nc"
# [2] "OR_ABI-L2-AODC-M3_G16_s20190901607158_e20190901609530_c20190901611598.nc"
# [3] "OR_ABI-L2-AODC-M3_G16_s20190901612158_e20190901614530_c20190901616568.nc"
# [4] "OR_ABI-L2-AODC-M3_G16_s20190901617158_e20190901619530_c20190901621597.nc"
# [5] "OR_ABI-L2-AODC-M3_G16_s20190901622158_e20190901624530_c20190901626585.nc"
# [6] "OR_ABI-L2-AODC-M3_G16_s20190901627158_e20190901629530_c20190901632028.nc"
# [7] "OR_ABI-L2-AODC-M3_G16_s20190901632158_e20190901634530_c20190901637092.nc"
# [8] "OR_ABI-L2-AODC-M3_G16_s20190901637157_e20190901639530_c20190901642057.nc"
# [9] "OR_ABI-L2-AODC-M3_G16_s20190901642157_e20190901644530_c20190901646588.nc"
# [10] "OR_ABI-L2-AODC-M3_G16_s20190901647157_e20190901649530_c20190901652035.nc"
# [11] "OR_ABI-L2-AODC-M3_G16_s20190901652157_e20190901654530_c20190901657079.nc"
# [12] "OR_ABI-L2-AODC-M3_G16_s20190901657157_e20190901659530_c20190901701594.nc"

# Resolved! Changed regex to:
# "OR_ABI-L2-AODC-M[0-9]_G16_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
# from
# "OR_ABI-L2-AODC-M6_G16_s[0-9]+_e[0-9]+_c[0-9]+\\.nc"
