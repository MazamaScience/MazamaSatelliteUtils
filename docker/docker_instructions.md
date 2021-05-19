# Docker Instructions
Make sure you `cd` into the `docker/` directory!

## Build the Docker image
```
make production_build
```

## Run Container and Open a Console
Change the volume paths accordingly.
```
docker run \
  --rm \
  -it \
  --volume="/Users/mazama/Projects/MazamaSatelliteUtils/executables:/app" \
  --volume="/Users/mazama/Data/Satellite:/home/mazama/data/Satellite" \
  mazamascience/mazamasatelliteutils \
  /bin/bash
```

# Run Animation Script Through the Container
Change the volume paths accordingly.
```
docker run \
  --rm \
  --volume="/Users/mazama/Projects/MazamaSatelliteUtils/executables:/app" \
  --volume="/Users/mazama/Data/Satellite:/home/mazama/data/Satellite" \
  mazamascience/mazamasatelliteutils \
  /app/goesaodc_animateScanRasters_exec.R \
    --satID="G16" \
    --starttime="2021-05-18 09:00" \
    --endtime="2021-05-18 10:00" \
    --timezone="America/Los_Angeles" \
    --bbox="-125.5, -113.5, 31.5, 42.5" \
    --cellSize=0.1 \
    --rasterAlpha=1 \
    --legendLimits="-0.5, 5.5" \
    --includeMap=FALSE \
    --zoom=8 \
    --stateCodes="CA" \
    --satelliteDataDir="/home/mazama/data/Satellite" \
    --spatialDataDir="/home/mazama/data/Spatial" \
    --frameRate=10 \
    --outputDir="/home/mazama/data/Satellite" \
    --logDir="/app" \
    --verbose=TRUE
```
