library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

ncFiles <- goesaodc_listFiles("G17", 2019090202)
ncHandle <- goesaodc_openFile(ncFiles[1])

# Fill in (by row) gaps with up to 2 NA points
aod <- as.vector(ncdf4::ncvar_get(ncHandle, "AOD"))
filledAod <- zoo::na.locf0(aod, maxgap = 2)

# ----- Project spatial points ---------------------------------------------

varList <- list()
varList[["AOD"]] <- 10 ^ filledAod
varList[["lon"]] <- as.numeric(MazamaSatelliteUtils::goesWestGrid$longitude)
varList[["lat"]] <- as.numeric(MazamaSatelliteUtils::goesWestGrid$latitude)

aodTbl <- 
  tibble::as_tibble(varList) %>% 
  tidyr::drop_na()

regionBbox <- 
  maps::map("state", regions = c("oregon"), fill = TRUE, plot = FALSE)$range
aodTbl <- dplyr::filter(aodTbl, 
                        lon >= regionBbox[1], lon <= regionBbox[2],
                        lat >= regionBbox[3], lat <= regionBbox[4])

# Limit AOD values to the color scale range
aodTbl$AOD[aodTbl$AOD > 4.5] <- 4.5

# ----- Draw frame ---------------------------------------------------------

breaks <- c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5)

sp <- sp::SpatialPointsDataFrame(
  coords = dplyr::select(aodTbl, c(.data$lon, .data$lat)),
  data = dplyr::select(aodTbl, -c(.data$lon, .data$lat))
)

maps::map("state", regions = "oregon", col = NA)
goesaodc_plotSpatialPoints(sp, var = "AOD", cex = 0.5, breaks = breaks, add = TRUE)
maps::map("state", regions = "oregon", lwd = 2.0, add = TRUE)


