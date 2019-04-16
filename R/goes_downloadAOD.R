#' @export
#' @title download GOES 16 AOD data
#' @param date desired date (integer, character representing YYYYMMDD[HH] or datetime object)
#' @param time UTC hour and minute for data (HHMM)
#' @param julianDate desired date on Julian calendar (YYYYDDD). Ignored if 'date' is specified.
#' @param baseUrl base URL for data queries
#' @return 0 if successful
#' 

goes_downloadAOD(
  date = NULL,
  time = NULL,
  julianDate = NULL,
  baseUrl = "https://tools-1.airfire.org/Satellite/GOES-16/AODC/"
) {
  # Sanity check
  
  # Parse date and time
  
  # Create URL
  
  # Get satelliteDataDir
  
  # Check if file exists locally
  
    # if not, download file
  
  return(NULL)
}
