# Used to generate data for plot_monitors_with_cartopy_2019-04-05.ipynb

# Load monitoring data for the hour in which AOD was sampled, and print the data
# out as a CSV.

# Scan start time for AOD data in
# OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc
#
# 2019-03-19 15:12:18.600000

library(PWFSLSmoke)

datetime = 2019031915

monitor <- monitor_load(datetime, datetime)

data <- tibble(
  latitude = monitor$meta$latitude,
  longitude = monitor$meta$longitude,
  value = monitor$data[,2:length(monitor$data)] %>% as.numeric()
)

readr::write_csv(data, sprintf("monitors_%d.csv", datetime))

