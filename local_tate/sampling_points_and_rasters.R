# Get the AOD value of the cell closest to given coordinates
raster::extract(
  x = oregonRaster,
  y = data.frame(lon = -124, lat = 44), 
  method = "simple",
  buffer = 1,
  fun = mean
)[,"AOD"]