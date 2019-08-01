# In goesaodc_getProjection:
library(MazamaSpatialUtils)
library(MazamaSatelliteUtils)

setSpatialDataDir('~/Data/Spatial')
setSatelliteDataDir('./local_data/CampFire/001')

nc <- goesaodc_openFile(filename = "OR_ABI-L2-AODC-M3_G16_s20183192207157_e20183192209530_c20183192212207.nc")
#nc <- example_nc
#nc  <- working_nc

varid <- "goes_imager_projection"

varToUse <- -1
if (nc$nvars > 0) {
  for (kk in 1:nc$nvars) {
    if (varid == nc$var[[kk]]$name) {
      varToUse <- kk
    }
  }
}



print(nc$var[[varToUse]]$longname)
print(nc$var[[varToUse]]$id$group_id)

# Back in ncatt_get()
idobj <- varid
#if ( verbose ) print('ncatt_get: calling ncatt_get_inner for a non-global att')
# Calls ncatt_get_inner(idobj$group_id, idobj$id, attname=attname, verbose=verbose)

# In ncatt_get_inner()
ncid <- idobj$group_id
varid <- idobj$id
attname <- NA

#if (verbose) print(paste("ncatt_get_inner: entering with ncid=", ncid, "varid=", varid, "attname=", attname ))

retval <- list()

#if( verbose ) print(paste("ncatt_get_inner: no attname specified, returning a list with name/value pairs *******"))
# Calls ncatt_get_n(ncid, varid)

# In ncatt_get_n()
nc <- ncid
#if( verbose ) print(paste("ncatt_get_n: entering with integer (ONLY) nc=", nc, "and integer (ONLY) varid=",varid))
#if( verbose ) print ("ncatt_get_n: varid != -1, so getting number of attributes for a specific var")
# Calls ncvar_ndims(nc, varid)

# In ncvar_ndims()
ncid <- nc
varid <- varid

rv <- list()
rv$error <- -1
rv$ndims <- -1

#print(paste("ncid:", ncid, "varid:", varid))







