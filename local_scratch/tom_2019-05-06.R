# Tom's scratch
# 05/06/2019

# ----- using profvis package to profile goesaodc_createSpatialPoints ----------
# profvis package: https://github.com/rstudio/profvis

date <- 20190506
hour <- 12
startdate <- paste0(date, hour)

goesaodc_downloadAOD(date, hour)

nc <- 
  goesaodc_listFiles(startdate)[1] %>%
  goesaodc_openFile()

# This doesnt work...
# getting error:
#   profvis: code exited with error:
#   argument is of length zero
# 
#   Error in parse_rprof(prof_output, expr_source) : 
#     No parsing data available. Maybe your function was too fast?
profvis::profvis({
  pts <- goesaodc_plotSpatialPoints(nc)
})

# Try the example from profvis README
library(ggplot2)

profvis::profvis({
  g <- ggplot(diamonds, aes(carat, price)) + geom_point(size = 1, alpha = 0.2)
  print(g)
})

# that works.

# maybe I need to source the function?
source(paste0(getwd(), "/R/goesaodc_plotSpatialPoints.R"))

