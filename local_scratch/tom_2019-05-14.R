# Tom's scratch
# 5/14/19

# What is AOD?

# - Data product produced from reflectances observed by the Advanced Baseline
# Imager aboard the GOES-R geostationary satellite.
# - Measures atmospheric aerosol properties at a large scale
# - Can AOD be used as a proxy for air quality at the surface of the earth?

# https://www.accuweather.com/en/weather-news/strong-dry-winds-buffet-growing-wildfire-in-southern-new-jersey/70007854

# Brushfire in New Jersey sparked on March 30th, 2019

# Download AOD data
date <- 20190331
hour <- 16

goesaodc_downloadAOD(date = date, hour = hour)

# is it daylight?
isDaylight(paste(date, hour))

# Plot AOD
rst <- goesaodc_createHourlyRasterStack(paste0(date, hour))


getSatelliteDataDir() %>%
  list.files() %>%
  stringr::str_subset(format(dt, "%Y%j"))


