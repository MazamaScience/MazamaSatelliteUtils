#!/bin/bash

# Animate 6 hours 12 hours ago (ex. if it is 9pm, animate 9am-3pm). Weird time 
# range b/c GOES scans become available from AirFire several hours late.

# Using UTC
START_TIME=$(date -u -v-12H +"%Y-%m-%d %H:00:00")
END_TIME=$(date -j -v+6H -f "%Y-%m-%d %H:%M:%S" "$START_TIME" +"%Y-%m-%d %H:%M:%S")

echo "Generating animation for ${START_TIME}" >> /Users/mazama/Projects/MazamaSatelliteUtils/local_tate/output.log

/usr/local/bin/docker run \
  --rm \
  --volume="/Users/mazama/Projects/MazamaSatelliteUtils/executables:/app" \
  --volume="/Users/mazama/Data/Satellite:/home/mazama/data/Satellite" \
  mazamascience/mazamasatelliteutils \
  /app/goesaodc_animateScanRasters_exec.R \
    --satID="G16" \
    --starttime="$START_TIME" \
    --endtime="$END_TIME" \
    --timezone="UTC" \
    --bbox="-122.5, -111.5, 34.5, 42.5" \
    --cellSize=0.05 \
    --legendLimits="-0.1, 5.1" \
    --stateCodes="NV" \
    --satelliteDataDir="/home/mazama/data/Satellite" \
    --spatialDataDir="/home/mazama/data/Spatial" \
    --outputDir="/home/mazama/data/Satellite" \
    --logDir="/app" \
    --frameRate=10 \
    --verbose=TRUE

echo "Completed animation for ${START_TIME}" >> /Users/mazama/Projects/MazamaSatelliteUtils/local_tate/output.log