setSatelliteDataDir("~/Data/Satellite")

dqfLevel <- 2
frameRate <- 3
outputDir <- "~/Desktop/"

if (dqfLevel == 0) {
  dqfTitle <- "High"
} else if (dqfLevel == 1) {
  dqfTitle <- "Medium & High"
} else {
  dqfTitle <- "Low & Medium & High"
}

# Define date range and time step
start <- lubridate::ymd_h("2019-05-01 16", tz = "UTC")
end <- lubridate::ymd_h("2019-05-25 16", tz = "UTC")
times <- seq.POSIXt(from = start, to = end, by = "day")

frameNumber <- 1
for (i in 1:length(times)) {
  
  # Load day data
  #goesaodc_downloadAOD(startdate = times[i])
  print(times[i])
  timeFiles <- goesaodc_listFiles(startdate = times[i])
  
  nc <- goesaodc_openFile(filename = timeFiles[1])
  sp <- goesaodc_createSpatialPoints(nc, dqfLevel = dqfLevel)
  
  n <- stringr::str_pad(frameNumber, 3, 'left', '0')
  frameFilePath <- paste0(tempdir(), "/", n, ".png")
  
  localTime <- times[i]
  attributes(localTime)$tzone = "America/Los_Angeles"

  # Draw frame
  png(frameFilePath, width = 1280, height = 720, units = "px")
  maps::map("state")
  goesaodc_plotSpatialPoints(sp, var = "DQF", cex = 0.4, add = TRUE)
  
  title(paste0("Data Quality: ", dqfTitle,"\n", localTime, " PDT"), cex.main = 2.0)
  if (dqfLevel < 2) {
    mtext("No data for the NW beyond a certain radius", 
          side = 1, line = 1.0, cex = 1.6)
  }
  
  dev.off()
  
  frameNumber <- frameNumber + 1
}

# Create video from frames
cmd_cd <- paste0("cd ", tempdir())
cmd_ffmpeg <- paste0("ffmpeg -loglevel quiet -r ", 
                     frameRate, " -f image2 -s 1280x720 -i ",
                     "%03d.png -vcodec libx264 -crf 25 ", 
                     outputDir, "USA_DQF", dqfLevel, ".mp4")
cmd_rm <- paste0("rm *.png")
cmd <- paste0(cmd_cd, " && ", cmd_ffmpeg)

system(cmd)
