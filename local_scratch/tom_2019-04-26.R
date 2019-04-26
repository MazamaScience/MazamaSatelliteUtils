# 04/26/2019 Scratch

# ----- get scan start time ----------------------------------------------------

dataFiles <- list.files(getSatelliteDataDir())[13:51]

getStartTime <- function(file) {
  stringr::str_split(file, "_") %>% 
    unlist() %>% 
    dplyr::nth(-3) %>% 
    stringr::str_sub(2, -1)
}

startTimes <- purrr::map_chr(dataFiles, getStartTime)


mask <- stringr::str_detect(startTimes, "201910509")
matchingFiles <- dataFiles[mask]

# ----- plot SpatialPoints -----------------------------------------------------

# Jon's example
indices <- sample(seq_len(nrow(pts)), 1e4)
pts_sub <- pts[indices,]
cols <- brewer.pal(5, "YlOrRd")
col_i <- .bincode(pts_sub[["AOD"]], c(-10, 0.2, 1, 1.5, 2, 10))
col_v <- cols[col_i]
plot(pts_sub, pch=15, col=col_v, cex=0.5)

# draft function
plotSpatialPoints <- function(
  pts,
  var = "AOD",
  n = 1e5,
  colBins = 5,
  breaks = NULL,  # overrides colBins
  pch = 15,
  cex = 0.5,
  paletteName = "YlOrRd"
) {
  
  # Subsample points
  indices <- sample(seq_len(nrow(pts)), n)
  ptsSub <- pts[indices,]

  # Make breaks for specifed number of equally sized color bins
  # TODO: Use quantiles
  if (is.null(breaks)) {
    mn <- min(ptsSub[[var]])
    mx <- max(ptsSub[[var]])
    range <- mx - mn
    
    breaks <- c(mn)
    for (i in 1:colBins) {
      breaks <- c(breaks, mn + i*(range/colBins))
    }
  }
  
  cols <- brewer.pal(length(breaks)-1, "YlOrRd")
  col_i <- .bincode(ptsSub[[var]], breaks)
  col_v <- cols[col_i]
  plot(ptsSub, pch=pch, col=col_v, cex=cex)
}

plotSpatialPoints(pts, breaks=c(-10, 0.2, 1, 1.5, 2, 10))
