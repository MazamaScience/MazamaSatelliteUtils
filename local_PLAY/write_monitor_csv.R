# Used to generate data for plot_monitors_with_cartopy_2019-04-05.ipynb

# Load monitoring data for the hour in which AOD was sampled, and print the data
# out as a CSV.

# Scan start time for AOD data in
# OR_ABI-L2-AODC-M3_G16_s20190781512186_e20190781514559_c20190781516459.nc
#
# 2019-03-19 15:12:18.600000

library(PWFSLSmoke)
library(lubridate)

datetime <- "201903191512"

aod_datetime <- as_datetime(datetime,
                            format="%Y%m%d%H%M")

# Retrieve data for 3 days preceding the time that the AOD data was collected
# to apply nowcast algorithm
monitor <- monitor_load(aod_datetime - days(3), aod_datetime)

# apply nowcast algorithm and retrieve the hour associated with AOD data
monitor_data <-
  monitor_nowcast(monitor) %>%
  monitor_extractData() %>%
  dplyr::filter(datetime == floor_date(aod_datetime, unit = "hours")) %>%
  dplyr::select(-datetime) %>%
  as.numeric()

tbl <- tibble(
  latitude = monitor$meta$latitude,
  longitude = monitor$meta$longitude,
  value = monitor_data
)

readr::write_csv(tbl, sprintf("monitors_%s.csv", datetime))

