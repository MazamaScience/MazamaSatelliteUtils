# Tom's Scratch
# 5/17/19

# ----- Inspecting AOD from the Camp Fire --------------------------------------

# I want to put of the tools we've been developing to a practical use, so I am
# going to explore AOD in California during the Camp Fire last year. I requested
# and downloaded AOD files produced from GOES 16 reflectances on November 15th,
# when AQI was the highest in California during the period of time that the
# Camp Fire was burning.

# Lets see exactly what time period the files I downloaded covers

setSatelliteDataDir("~/Data/Satellite/CampFire/001")

times <-
  list.files(getSatelliteDataDir()) %>%
  purrr::map(goesaodc_getStartString) %>%
  purrr::map(lubridate::parse_date_time, orders = "YjHMS", tz = "UTC")

# first file in california local time
firstTime <- times[[1]]
lubridate::with_tz(firstTime, "US/Pacific")
# "2018-11-15 06:02:15 PST"

lastTime <- times[[length(times)]])
lubridate::with_tz(unlist(times[[length(times)]]), "US/Pacific")
# "2018-11-15 16:57:15 PST"

# ok lets just try making a RasterStack for the first hour

setSpatialDataDir("~/Data/Spatial")

# get bounding box for Pennsylvania
loadSpatialData("USCensusStates")
ca <- subset(USCensusStates, stateCode == "CA")
bb_ca <- bbox(ca)

rstStack <- goesaodc_createHourlyRasterStack("2018111514", bbox = bb_ca)
