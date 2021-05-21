#!/bin/bash

# Raster animations of Nevada's Cherrywood fire (37.25 N, 116.25 W) on 
# 2021-05-19.
# 3 different region sizes: 5x5, 2x2, 1x1 degrees

SAT_ID="G16"
START_TIME="2021-05-19 13:00"
END_TIME="2021-05-19 19:00"
TIMEZONE="America/Los_Angeles"
CELL_SIZE=0.05
RASTER_ALPHA=0.5
LEGEND_LIMITS="-0.1, 5.1"

# 5x5 deg region animation
/usr/local/bin/docker run \
  --rm \
  --volume="/Users/mazama/Projects/MazamaSatelliteUtils/executables:/app" \
  --volume="/Users/mazama/Data/Satellite:/home/mazama/data/Satellite" \
  mazamascience/mazamasatelliteutils \
  /app/goesaodc_animateScanRasters_exec.R \
    --satID="$SAT_ID" \
    --starttime="$START_TIME" \
    --endtime="$END_TIME" \
    --timezone="$TIMEZONE" \
    --bbox="-118.5, -113.5, 35.5, 40.5" \
    --cellSize=$CELL_SIZE \
    --rasterAlpha=$RASTER_ALPHA \
    --legendLimits="$LEGEND_LIMITS" \
    --includeMap=TRUE \
    --zoom=7 \
    --satelliteDataDir="/home/mazama/data/Satellite" \
    --spatialDataDir="/home/mazama/data/Spatial" \
    --outputDir="/home/mazama/data/Satellite" \
    --logDir="/" \
    --outputFilename="cherrywood_fire_5x5" \
    --frameRate=10 \
    --verbose=TRUE

# 2x2 deg region animation
/usr/local/bin/docker run \
  --rm \
  --volume="/Users/mazama/Projects/MazamaSatelliteUtils/executables:/app" \
  --volume="/Users/mazama/Data/Satellite:/home/mazama/data/Satellite" \
  mazamascience/mazamasatelliteutils \
  /app/goesaodc_animateScanRasters_exec.R \
    --satID="$SAT_ID" \
    --starttime="$START_TIME" \
    --endtime="$END_TIME" \
    --timezone="$TIMEZONE" \
    --bbox="-117.5, -113.5, 35.0, 39.0" \
    --cellSize=$CELL_SIZE \
    --rasterAlpha=$RASTER_ALPHA \
    --legendLimits="$LEGEND_LIMITS" \
    --includeMap=TRUE \
    --zoom=8 \
    --satelliteDataDir="/home/mazama/data/Satellite" \
    --spatialDataDir="/home/mazama/data/Spatial" \
    --outputDir="/home/mazama/data/Satellite" \
    --logDir="/" \
    --outputFilename="cherrywood_fire_2x2" \
    --frameRate=10 \
    --verbose=TRUE
    
# 1x1 deg region animation
/usr/local/bin/docker run \
  --rm \
  --volume="/Users/mazama/Projects/MazamaSatelliteUtils/executables:/app" \
  --volume="/Users/mazama/Data/Satellite:/home/mazama/data/Satellite" \
  mazamascience/mazamasatelliteutils \
  /app/goesaodc_animateScanRasters_exec.R \
    --satID="$SAT_ID" \
    --starttime="$START_TIME" \
    --endtime="$END_TIME" \
    --timezone="$TIMEZONE" \
    --bbox="-116.75, -115.75, 36.75, 37.75" \
    --cellSize=$CELL_SIZE \
    --rasterAlpha=$RASTER_ALPHA \
    --legendLimits="$LEGEND_LIMITS" \
    --includeMap=TRUE \
    --zoom=9 \
    --satelliteDataDir="/home/mazama/data/Satellite" \
    --spatialDataDir="/home/mazama/data/Spatial" \
    --outputDir="/home/mazama/data/Satellite" \
    --logDir="/" \
    --outputFilename="cherrywood_fire_1x1" \
    --frameRate=10 \
    --verbose=TRUE
    