# Occasionally a scan file can't be opened due to an "HDF error":

# Download the faulty file
scanFiles <- goesaodc_downloadAOD(
  satID = "G17",
  datetime = "2020-09-08 15",
  timezone = "America/Los_Angeles"
)

ncdf4::nc_open("~/Data/Satellite/OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc")

# Error in R_nc4_open: NetCDF: HDF error
# Error in ncdf4::nc_open("~/Data/Satellite/OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc") : 
#   Error in nc_open trying to open file /Users/Tate/Data/Satellite/OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc

# Set data directories
setSpatialDataDir("~/Data/Spatial")
setSatelliteDataDir("~/Data/Satellite")

# Load state polygons
loadSpatialData("USCensusStates")
loadSpatialData("NaturalEarthAdm1")

# Draw plot
goesaodc_plotScanPoints(
  filename = "/Users/Tate/Data/Satellite/OR_ABI-L2-AODC-M6_G17_s20202522231174_e20202522233547_c20202522235327.nc ",
  bbox = c(-125, -116, 42, 47),
  breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf),
  stateCodes = "OR"
)


