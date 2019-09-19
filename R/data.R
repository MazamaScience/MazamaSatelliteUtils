#' Sample NetCDF files of GOES 16 and 17
#'
#' 2 files which contain AOD data from GOES 16 and GOES 17 for the 
#' time of 1826 hours on September 6, 2019.
#'
#' @format ABI L2+ AOD NetCDF files from NOAA which contains the aerosol optical 
#' depth at 550 nm over land and over water,associated quality flags, mean, 
#' maximum, minimum and standard deviation of 550-nm AOD for the entire domain. 
#' The AOD is a measure of the columnar extinction (scattering +absorption) of 
#' radiation by aerosols. It is proportional to the amount (number or mass 
#' concentration) of aerosols in an atmospheric column. 
#' \describe{
#'   \item{dqf}{data quality}
#'   \item{aod}{measure optical depth}
#'   ...
#' }
#' @source \url{https://www.ncdc.noaa.gov/sites/default/files/attachments/GOES-17_ABI_L2_AOD_Provisional_ReadMe.pdf}
"netcdf"
