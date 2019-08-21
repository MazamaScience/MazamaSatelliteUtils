#!/usr/local/bin/Rscript

# This Rscript generates a raster video for a state over a given day. If
# The resulting video is labeled by the state, state, variable, and DQF level.
#
# Test this script from the command line with:
#
# ./createRasterVideo_exec.R -d 20190801 -s OR -x AOD -q 2 -r 4 -o ~/Desktop/ -v TRUE

VERSION = "0.1.1"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(MazamaCoreUtils)
  library(MazamaSatelliteUtils)
  library(MazamaSpatialUtils)
})