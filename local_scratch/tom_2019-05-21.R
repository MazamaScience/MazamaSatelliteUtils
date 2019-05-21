# Tom's scratch
# 5/21/19

# ----- Overlay California on rasterVis::levelplot -----------------------------

# Yesterday I fixed the bug that was preventing me from creating a raster
# with the data for 15:00 Nov 15 2018 UTC (Camp Fire)

# Now I'll try and produce a plot with rasterVis::levelplot that overlays a 
# California polygon over each facet in the plot

library(MazamaSpatialUtils)

setSatelliteDataDir("./local_data/CampFire/001")
setSpatialDataDir("~/Data/Spatial")

loadSpatialData("USCensusStates")

ca <- subset(USCensusStates, stateCode == "CA")
bb_ca <- sp::bbox(ca)

startdate <- "2018111515"
rstStack <- goesaodc_createHourlyRasterStack(startdate, bbox = bb_ca)

plot <- rasterVis::levelplot(rstStack)
plot + latticeExtra::layer(sp.lines(ca))

# Cool beans that worked exactly how I had hoped

# ----- Small multiples with ggplot --------------------------------------------

  # ----- Resetting graphics device --------------------------------------------

dev.off()
  
  # ----- plotting -------------------------------------------------------------

library(ggplot2)

# create a list of SpatialPointsDataFrames for startdate
pts <-
  goesaodc_listFiles(startdate) %>%
  purrr::map(goesaodc_openFile) %>%
  purrr::map(goesaodc_createSpatialPoints, bbox = bb_ca)

ca_df <- broom::tidy(ca)

ggplot(data = ca_df) +
  geom_point(mapping = aes(x = long, y = lat))
  

