# ----- Faulty scan example ----------------------------------------------------

# Occasionally a scan file can't be opened due to an "HDF error":

# Set data directories
setSpatialDataDir("~/Data/Spatial")
setSatelliteDataDir("~/Data/Satellite")

# Download a faulty file
scanFiles <- goesaodc_downloadAOD(
  satID = "G17",
  datetime = "2020-09-08 15",
  timezone = "America/Los_Angeles"
)

# Try to open it
ncdf4::nc_open("~/Data/Satellite/OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc")

# Error in R_nc4_open: NetCDF: HDF error
# Error in ncdf4::nc_open("~/Data/Satellite/OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc") : 
#   Error in nc_open trying to open file /Users/Tate/Data/Satellite/OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc

# ----- Handling suggestions ---------------------------------------------------

# We should handle creating SPDFs, rasters, and plots from faulty scans in a 
# intuitive and useful way rather than just halting execution:

# When given a faulty scan, goesaodc_createScanSPDF() SHOULD return an SPDF with
# the standard point coords for the satellite, but all data values should be NA.
goesaodc_createScanSPDF(
  filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc"
)

# When given a faulty scan, goesaodc_createScanRaster() SHOULD return a Raster 
# with the standard grid for the satellite/resolution, but all cells data should
# be NA.
goesaodc_createScanRaster(
  filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
  cellSize = 0.05
)

# When given a faulty scan, goesaodc_plotScanPoints() should return a plot of NA
# points. (What about the color scale legend? It can't be drawn when there are
# no continuous values...)
goesaodc_plotScanPoints(
  filename = "OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc",
  bbox = c(-125, -116, 42, 46.5),
  breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
  stateCodes = "OR"
)
