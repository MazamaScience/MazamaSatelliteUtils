#' @title Map raw maiac data
#' @param maiac_raw raw maiac data (created from maiac_loadRawDataframe)
#' @param param data parameter to plot
#' @param xlim longitude range to be plotted
#' @param ylim latitude range to be plotted
#' @param breaks used by \code{image()} function to partition values into different colors
#' @param colors vector of colors to use
#' @param addAxes logical specifying whether to add axes
#' @param main title for the plot
#' @param pch point type for grid pixels
#' @param cex cex for grid pixels
#' @param ... additional arguments passed to maps::map()
#' @description Plots a map of raw maiac data for a given area
#' @examples
#' \dontrun{
#' 
#' }

gridMap <- function(maiac_raw, param="aot_055",
                    xlim=range(maiac_raw$longitude), ylim=range(maiac_raw$latitude), 
                    breaks=NULL, colors=NULL, naColor="gray80", addAxes=TRUE, main=NULL, 
                    pch = 17, cex = .5, ...) {
  
  # subset bs grid if necessary
  maiac_sub <- maiac_cropRaw(maiac_raw, xlim, ylim)
  lon <- maiac_sub$longitude
  lat <- maiac_sub$latitude
  data <- maiac_sub[[param]]
  
  
  # new breaks logic
  if ( is.null(breaks) && is.null(colors) ) {
      # NOTE: Colors and breaks are the same as those used at https://www.airfire.org/data/bluesky-daily/
      breaks <- c(0, 0.01, 0.03, 0.06,  0.1, 0.2, 0.3, 3)
      colors <- colorRampPalette(c("white", "orange", "red"))(9)
  }
  
  # create map
  
  colorIndex <- .bincode(data, breaks)
  col <- colors[colorIndex]
  col <- ifelse(is.na(col), naColor, col)
  plot(lon, lat, col = col, pch = pch, cex = cex)
  maps::map('state', add = TRUE)
  title(main=main)
  result <- try( maps::map('county', col='gray70', xlim=xlim, ylim=ylim, add=TRUE),
                 silent=TRUE )
  result <- try( maps::map('state', xlim=xlim, ylim=ylim, add=TRUE),
                 silent=TRUE )
}
