#' @export
#' 
#' @title Calculate the trend of a series of GOES scan points
#' 
#' @description Creates a \code{SpatialPointsDataFrame} of AOD reading trends 
#' from a \code{list} of GOES scan \code{SpatialPointsDataFrame}s. A trend value
#' for a point is calculated by taking the difference between it's average value
#' in the first half of the scan series and it's average value in the second.
#' 
#' @param spdfList A \code{list} of GOES scan \code{SpatialPointsDataFrame}s.
#' @param na.rm Logical flag whether to remove NA values before calculating the
#' trend. Defaults to \code{FALSE}.
#' 
#' @examples
#' \donttest{
#' library(MazamaSatelliteUtils)
#' setSatelliteDataDir("~/Data/Satellite")
#' 
#' bboxOregon <- c(-125, -116, 42, 47)
#' 
#' scanFiles <- goesaodc_listScanFiles(
#'   satID = "G17",
#'   datetime = "2020-09-08 12:00",
#'   endtime = "2020-09-08 13:00",
#'   timezone = "America/Los_Angeles"
#' )
#' 
#' scanPointsList <- goesaodc_createScanPoints(
#'   filename = scanFiles,
#'   bbox = bboxOregon
#' )
#' 
#' goesaodc_calcTrendScanPoints(
#'   spdfList = scanPointsList,
#'   na.rm = TRUE
#' )
#' }

goesaodc_calcTrendScanPoints <- function(
  spdfList = NULL,
  na.rm = FALSE
) {
  
  middleScanIndex <- floor(length(spdfList) / 2)
  
  half1Avg <- goesaodc_calcAverageScanPoints(
    spdfList[1:middleScanIndex],
    na.rm = na.rm
  )$AOD
  
  half2Avg <- goesaodc_calcAverageScanPoints(
    spdfList[(middleScanIndex + 1):length(spdfList)],
    na.rm = na.rm
  )$AOD
  
  trendValues <- 
    tibble::tibble(half1Avg, half2Avg) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(trend = .data$half2Avg - .data$half1Avg) %>%
    dplyr::pull(.data$trend)
  
  trendSpdf <- sp::SpatialPointsDataFrame(
    coords = spdfList[[1]]@coords,
    data = data.frame(
      AOD = trendValues
    )
  )
  
  return(trendSpdf)
  
}
