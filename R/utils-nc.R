#' @export
#'
#' @title Returns attributes from a netCDF file
#' 
#' @description Returns attributes from a netCDF file.
#'
#' @param nc ncdf4 handle.
#' @param varid The variable whose attribute is to be read. Can be a character 
#' string with the variable's name or an object of class ncvar4. As a special 
#' case, if \code{varid == 0}, then a global (file) attribute will be read 
#' rather than a particular variable's attribute.
#' @param verbose Logical flag to print attribute structure. Defaults to TRUE.
#'
#' @return Invisibly returns a named list of netCDF attributes.

getAttributes <- function(
  nc,
  varid = 0,
  verbose = TRUE
) {
  
  attributeList <- ncdf4::ncatt_get(nc, varid)
  
  if ( verbose ) {
    utils::str(attributeList)
  }
  
  return(invisible(attributeList))
  
}

