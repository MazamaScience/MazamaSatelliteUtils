# ----- Bluesky breaks -------------------------------------------------------------

#' Bluesky breaks and associated colors
#' 
#' @docType data
#' @name bs_colors
#' @title Bluesky Model Levels and Colors
#' @format A list with two elements
#' @export
#' @description
#' Levels and colors are provided in a list for easy coloring in maps or elsewhere.
#' @details
#' 
#' Colors and breaks are as used in Bluesky Daily Run maps as of Augus 2017:
#' \url{https://www.airfire.org/data/bluesky-daily/}
#' @note
#' The low end of each break category is used as the breakpoint.

bs_colors <- list(breaks=c(-1, 1, 5, 10, 20, 40, 90, 140, 350, 525, 10000),
                  colors=c('transparent',
                           grDevices::rgb(red=c(255,255,255,255,255,255,255,200,150)/255,
                                          green=c(225,195,165,135,105,75,46,2,3)/255,
                                          blue=c(225,195,165,135,105,75,45,3,3)/255)))

# ----- AOT breaks -------------------------------------------------------------

#' Aerosol Optical Thickness (AOT) breaks and associated colors
#' 
#' @docType data
#' @name aot_colors
#' @title AOT Levels and Colors
#' @format A list with two elements
#' @export
#' @description
#' Levels and colors are provided in a list for easy coloring in maps or elsewhere.
#' @note
#' The low end of each break category is used as the breakpoint.

# TODO: check breaks for appropriateness
# TODO: update to continuous color spectrum, rather than discrete?

# length_out <- 50
# left_colors <- c(colorRampPalette(c("transparent","Red"))(length_out), "darkred")
# left_breaks <- c(seq(0, 1, length.out = length_out+1),1.1)

aot_colors <- list(breaks=c(0, .05, .1, .2, .3, .4, .5, .75, 1, 1.1),
                   colors=c('white', RColorBrewer::brewer.pal(9,'Reds')))

# uncomment below to troubleshoot differences in colors between sideBySide and rasterLeaflet
# aot_colors$colors[4] <- "#0000ff"

# c(0, (10/c(c(2,1) %o% 10^(5:1))), 1.1)
