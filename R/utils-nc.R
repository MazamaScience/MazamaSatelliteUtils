#' @export
#'
#' @title Returns attributes from a netCDF file
#'
#' @param nc An object of class ncdf4 (as returned from nc_open), indicating 
#' what file to read from.
#' @param varid The variable whose attribute is to be read. Can be a character 
#' string with the variable's name or an object of class ncvar4. As a special 
#' case, if \code{varid == 0}, then a global (file) attribute will be read 
#' rather than aparticular variable's attribute.
#' @param print Logical specifying whether human readable output should be
#' printed to the console.
#' 
#' @description 
#'
#' @return A named list of netCDF attributes is returned invisibly.
#'
getAttributes <- function(
  nc,
  varid = 0,
  print = TRUE
) {
  
  attributeList <- ncdf4::ncatt_get(nc, varid)
  
  if ( print ) {
    utils::str(attributeList)
 }
  
  return(invisible(attributeList))
  
}

