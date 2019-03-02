#' @keywords utils
#' @export
#' @title Convert column-integrated PM2.5 values to AOD
#' @param pm25 vector of column-integrated pm25 values
#' @description Converts a vector of PM2.5 values to AOD values
#' @return vector of AOD values.

maiac_pm25ToAOD <- function(pm25) {
  bext <- 2.5 * 10^(-6) * pm25
  aod <- bext * 10000
  return(aod)
}

