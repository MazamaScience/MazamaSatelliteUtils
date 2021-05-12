#' @export
#' 
#' @title Calculate the trend SPDF of a series of GOES scan SPDFs
#' 
#' @description Creates a \code{SpatialPointsDataFrame} of AOD reading trends 
#' from a \code{list} of GOES scan \code{SpatialPointsDataFrame}s. A trend value
#' for a point is calculated by taking the difference between it's average value
#' in the first half of the scan series and it's average value in the second 
#' half.
#' 
#' @param spdfList A \code{list} of GOES scan \code{SpatialPointsDataFrame}s.
#' @param na.rm Logical flag determining whether to remove NA values before 
#' calculating the trend. Defaults to \code{FALSE}.
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
#' spdfList <- goesaodc_createScanSpdf(
#'   filename = scanFiles,
#'   bbox = bboxOregon
#' )
#' 
#' goesaodc_calcTrendScanSpdf(
#'   spdfList = spdfList,
#'   na.rm = TRUE
#' )
#' }

goesaodc_calcTrendScanSpdf <- function(
  spdfList = NULL,
  na.rm = FALSE
) {
  
  middleScanIndex <- floor(length(spdfList) / 2)
  
  # Calculate average SPDF for the 1st half of the series
  half1AvgSpdfAod <- goesaodc_calcAverageScanSpdf(
    spdfList[1:middleScanIndex],
    na.rm = na.rm
  )$AOD
  
  # Calculate average SPDF for the 2nd half of the series
  half2AvgSpdfAod <- goesaodc_calcAverageScanSpdf(
    spdfList[(middleScanIndex + 1):length(spdfList)],
    na.rm = na.rm
  )$AOD
  
  # Calculate point differences from the 1st and 2nd half average SPDFs
  trendValues <- 
    tibble::tibble(half1AvgSpdfAod, half2AvgSpdfAod) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(trend = .data$half1AvgSpdfAod - .data$half2AvgSpdfAod) %>%
    dplyr::pull(.data$trend)
  
  # Create trend SPDF with point differences
  trendSpdf <- sp::SpatialPointsDataFrame(
    coords = spdfList[[1]]@coords,
    data = data.frame(
      AOD = trendValues
    )
  )
  
  return(trendSpdf)
  
}
