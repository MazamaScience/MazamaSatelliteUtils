#' @export
#' @title Get metadata list from metadata string in .nc file
#' @param filePath path to .nc file
#' @description Get metadata list from metadata string in .nc file
#' @return metadata list
#' @examples 
#' \dontrun{
#' hdfPath <- maiac_downloadNorthAmerica("h01v04", 20171009, 2150)
#' ncdfPath <- maiac_2nc4(hdfPath)
#' maiac_getMetadata(ncdfPath)
#' }

maiac_getMetadata <- function(
  filePath
) {
  
  # Load data
  if ( !file.exists(filePath) ) {
    stop(paste0(filePath, " not found"))
  } else {
    nc <- ncdf4::nc_open(filePath) 
  }
  
  meta <- ncdf4::ncatt_get(nc, 0, "StructMetadata_0")$value
  metaVector <- stringr::str_split(meta, "\n")[[1]]
  metaList <- list(
    SwathStructure = NULL, 
    grid_1km = list(
      GridName=stringr::str_split(metaVector[5], '\"')[[1]][2],
      xdim=as.numeric(stringr::str_split(metaVector[6], '=')[[1]][2]),
      ydim=as.numeric(stringr::str_split(metaVector[7], '=')[[1]][2]),
      xmn=stringr::str_split(stringr::str_split(metaVector[8], '=')[[1]][2], ",")[[1]][1] %>% stringr::str_sub(2) %>% as.numeric(),
      ymx=stringr::str_split(stringr::str_split(metaVector[8], '=')[[1]][2], ",")[[1]][2] %>% stringr::str_sub(1, -2L) %>% as.numeric(),
      xmx=stringr::str_split(stringr::str_split(metaVector[9], '=')[[1]][2], ",")[[1]][1] %>% stringr::str_sub(2) %>% as.numeric(),
      ymn=stringr::str_split(stringr::str_split(metaVector[9], '=')[[1]][2], ",")[[1]][2] %>% stringr::str_sub(1, -2L) %>% as.numeric(),
      projection=stringr::str_sub(metaVector[10], 15),
      projParams=stringr::str_sub(metaVector[11], 14),
      sphereCode=stringr::str_sub(metaVector[12], 14),
      gridOrigin=stringr::str_sub(metaVector[13], 14)
    ),
    grid_5km = list(
      GridName=stringr::str_split(metaVector[62], '\"')[[1]][2],
      xdim=as.numeric(stringr::str_split(metaVector[63], '=')[[1]][2]),
      ydim=as.numeric(stringr::str_split(metaVector[64], '=')[[1]][2]),
      xmn=stringr::str_split(stringr::str_split(metaVector[65], '=')[[1]][2], ",")[[1]][1] %>% stringr::str_sub(2) %>% as.numeric(),
      ymx=stringr::str_split(stringr::str_split(metaVector[65], '=')[[1]][2], ",")[[1]][2] %>% stringr::str_sub(1, -2L) %>% as.numeric(),
      xmx=stringr::str_split(stringr::str_split(metaVector[66], '=')[[1]][2], ",")[[1]][1] %>% stringr::str_sub(2) %>% as.numeric(),
      ymn=stringr::str_split(stringr::str_split(metaVector[66], '=')[[1]][2], ",")[[1]][2] %>% stringr::str_sub(1, -2L) %>% as.numeric(),
      projection=stringr::str_sub(metaVector[67], 15),
      projParams=stringr::str_sub(metaVector[68], 14),
      sphereCode=stringr::str_sub(metaVector[69], 14),
      gridOrigin=stringr::str_sub(metaVector[70], 14)
    )
  )
  
  vars_1km <- c("Optical_Depth_047", "Optical_Depth_055", "AOT_Uncertainty", "FineModeFraction", "Column_WV",
                "Injection_Height", "AOT_QA", "AOT_MODEL")
  vars_5km <- c("cosSZA", "cosVZA", "RelAZ", "Scattering_Angle", "Glint_Angle")
  
  metaList$grid_1km$vars <- vars_1km
  metaList$grid_5km$vars <- vars_5km
  
  return(metaList)
  
}


