# ffmpeg -loglevel quiet -r 3 -f image2 -s 1280x720 -i %03d.png -vcodec libx264 -crf 25 ~/Desktop/USA_DQF0.mp4

setSatelliteDataDir("~/Data/Satellite")

dqfLevel <- 0

if (dqfLevel == 0) {
  dqfTitle <- "High"
} else if (dqfLevel == 1) {
  dqfTitle <- "Medium & High"
} else {
  dqfTitle <- "Low & Medium & High"
}

start <- lubridate::ymd_h("2019-05-01 16", tz = "UTC")
end <- lubridate::ymd_h("2019-05-25 16", tz = "UTC")
times <- seq.POSIXt(from = start, to = end, by = "day")

frameNumber <- 1
for (i in 1:length(times)) {
  
  #goesaodc_downloadAOD(startdate = times[i])
  print(times[i])
  timeFiles <- goesaodc_listFiles(startdate = times[i])
  
  nc <- goesaodc_openFile(filename = timeFiles[1])
  sp <- goesaodc_createSpatialPoints(nc, dqfLevel = dqfLevel)
  
  n <- stringr::str_pad(frameNumber, 3, 'left', '0')
  frameFilePath <- paste0("~/Desktop/frames/", n, ".png")
  
  localTime <- times[i]
  attributes(localTime)$tzone = "America/Los_Angeles"

  png(frameFilePath, width = 1280, height = 720, units = "px")
  maps::map("state")
  goesaodc_plotSpatialPoints(sp, cex = 0.4, add = TRUE)
  title(paste0("Data Quality: ", dqfTitle,"\n", localTime, " PDT"), cex.main = 2.0)
  mtext("No data for the NW beyond a certain radius", 
        side = 1, line = 1.0, cex = 1.6)
  
  dev.off()
  
  frameNumber <- frameNumber + 1
}