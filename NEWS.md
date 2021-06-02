# MazamaSatelliteUtils 0.4.18

* Minor fix to `goesaodc_sampleScanRaster()`.
* Updated `docker/Dockerfile`.

# MazamaSatelliteUtils 0.4.17

* Added Docker capabilities.
* Added `goesaodc_sampleScanRaster()` function.
* `goesaodc_createScanSpdf()` function no longer returns a single 
`SpatialPointsDataFrame` if one of many passed in scan files can't be read.
* Added `na.rm` parameter to `goesaodc_createScanRaster()` function.
* Added `outputFilename` parameter to `goesaodc_animateScan...exec` scripts.
* `goesaodc_animateScan...exec` scripts no longer require you to supply the
`stateCodes` parameter.
* Added 'Comparing G16 and G17' article.

# MazamaSatelliteUtils 0.4.16

* Added `naColor` parameter to `goesaodc_plotScan...()` functions.
* `goesaodc_animateScan...()` functions now display proper timezone in 
timestamp.
* Updated format of animation file names.
* Added the following articles:
  * GOES Coverage
  * GOES Scan Distortion
  * Comparing GOES and BlueSky
* Added 'GOES Coverage' slideshow document.
* Updated `goesaodc_...()` function docs.

# MazamaSatelliteUtils 0.4.15

* Renamed all `goesaodc_...Points()` functions to `goesaodc_...Spdf()`.
* Renamed `animateScanPoints_exec` and `animateScanRaster_exec` scripts to
`goesaodc_animateScanSpdfs_exec` and `goesaodc_animateScanRasters_exec`.
* Renamed `installGoesGrid()` function to `goesaodc_installGoesGrid()`.
* Added `legendTitle` parameter to `goesaodc_plotScan...()` functions.
* Added `paletteColors` parameter to `goesaodc_plotScan...()` functions.
* `goesaodc_createScanRaster()` function now produces a RasterLayer with the 
name "AOD".
* `goesaodc_calcTrendScan...()` functions produce data structures with the name
"aodTrend".
* `goesaodc_plotScan...()` functions now color points/cells based using the 
first variable in their respective SPDF/raster instead of hardcoding to AOD.
* `goesaodc_listScanFiles()` now returns `NULL` when no files are found for the
day of the requested `datetime`.
* Hid `createGoesGrid()` function.
* Updated lots of function documentation.
* Updated 'Visualizing GOES AOD' article.

# MazamaSatelliteUtils 0.4.14

* `goesaodc_createScanPoints()` and `goesaodc_createScanRaster()` functions no
longer take `satID`, `datetime`, `endtime`, and `timezone` parameters.
* Renamed the following functions:
  * `goesaodc_openFile()` to `goesaodc_openScanFile()`
  * `goesaodc_createTibble()` to `goesaodc_createScanTibble`
* Removed the following functions/scripts:
  * `goesaodc_listFiles()`
  * `goesaodc_listDaytimeFiles()`
  * `goesaodc_downloadAOD()`
  * `goesaodc_downloadDaytimeAOD()`
  * `goesaodc_findClosestScanFile()`
  * `goesaodc_createSpatialPoints()`
  * `goesaodc_createRaster()`
  * `goesaodc_createRasterStack()`
  * `goesaodc_createDaytimeRasterStack()` 
  * `goesaodc_plotSpatialPoints()`
  * `goesaodc_areaPlot()`
  * `raster_createLocationTimeseries()`
  * `createSpatialPointsVideo_exec.R`
* Removed the following articles:
  * Milepost 97 Example           
  * AOD Data Structures
  * Grid Sizes
  * Working With GOES AOD Rasters
* Updated 'Visualizing GOES AOD' article.

# MazamaSatelliteUtils 0.4.13

* Added `goesaodc_listScanFiles()` function.
* Added `goesaodc_downloadScanFiles()` function.
* Removed `goesaodc_findClosestScanFile()` function.

# MazamaSatelliteUtils 0.4.12

* `goesaodc_createScanPoints()` and `goesaodc_createScanRaster()` no longer 
store DQF.
* `goesaodc_createScanPoints()` function now returns a list of 
`SpatialPointsDataFrames` when multiple scans are requested.
* `goesaodc_createScanRaster()` function now returns a list of 
`RasterBricks` when multiple scans are requested.
* Added `goesaodc_calcAverageScanPoints()` function.
* Added `goesaodc_calcAveragecanRaster()` function.
* Added `goesaodc_calcTrendScanPoints()` function.
* Added `goesaodc_calcTrendScanRasters()` function.
* Removed 'Kincade Fire Example' article.
* Removed `createSpatialPointsVideo_exec` script.
* Removed `goesaodc_areaPlot()` function.
* Removed `goesaodc_createDaytimeRasterStack()` function.

# MazamaSatelliteUtils 0.4.11

* Renamed all `goesaodc_create*SPDF()` functions to `goesaodc_create*Points()`.
* `goesaodc_plotScanPoints()` function now take in an SPDF.
* `goesaodc_plotScanRaster()` function now take in a raster.
* Fixed option type parsing in animation scripts.
* Added to the 'Visualizing GOES AOD' article.

# MazamaSatelliteUtils 0.4.10

* Added `animateScanRaster_exec` executable script.
* Renamed `breaks` and `limits` params in `goesaodc_plotScanPoints()` and
`goesaodc_plotScanRaster()` functions to `paletteBreaks` and `legendLimits`.

# MazamaSatelliteUtils 0.4.9

* Removed `goesaodc_plotScanSPDF()` function.
* `goesaodc_createScanSPDF()` now returns an SPDF of NA values if it cannot read
the scan.
* Added map drawing functionality to `goesaodc_plotScanPoints()` function.
* Added `limits` param to `goesaodc_plotScanPoints()` function.
* Added `limits` param to `goesaodc_plotScanRaster()` function.

# MazamaSatelliteUtils 0.4.8

* Added `goesaodc_createScanRaster()`.
* Added `goesaodc_plotScanRaster()`.
* `goesaodc_createScanSPDF()` function now returns an empty 
SpatialPointsDataFrame when it cannot read scan files.
* DQF filtering is now performed when generating both single and averaged scan 
SPDFs, and no longer causes errors when DQF values are `NULL`.
* `dqfLevel` params across functions and scripts now default to 3.

# MazamaSatelliteUtils 0.4.7

* Added `animateScanPoints_exec` executable script.
* `goesaodc_plotScanPoints()` draws an empty plot when there is an error making
spatial points.
* `goesaodc_plotScanPoints()` no longer crashes when state codes are not 
provided.
* `goesaodc_plotScanSPDF()` now has default palette limits.

# MazamaSatelliteUtils 0.4.6

* Added `goesaodc_createScanSPDF()` function.
* Hid `goesaodc_createSingleScanSPDF()` and `goesaodc_createSingleScanSPDF()` 
functions.
* Renamed `goesaodc_singleScanToSPDF()` function to `goesaodc_createSingleScanSPDF()`.
* Renamed `goesaodc_multiScanToSPDF()` function to `goesaodc_createMultiScanSPDF()`.
* Added `pointShape`, `paletteName`, `stateCodes`, and `title` params to 
`goesaodc_plotScanPoints()`.
* Removed NA row dropping from `goesaodc_createTibble()`.
* Functions with a `dqfLevel` parameter now allowed it to be NULL.
* Updated low and high colors for the default palette used in 
`goesaodc_plotScanSPDF()`.
* Added "Plotting 2020 Oregon Wildfires" article.

# MazamaSatelliteUtils 0.4.5

* Added `goesaodc_plotScanSPDF()` function.
* Removed `goesaodc_plotAveragePoints()` function.
* Renamed `goesaodc_plotAODPoints()` function to `goesaodc_plotScanPoints`.
* Renamed `goesaodc_createSingleScanPoints()` function to `goesaodc_singleScanToSPDF()`.
* Renamed `goesaodc_createMultiScanPoints()` function to `goesaodc_multiScanToSPDF()`.
* Added **AirFirePlots** and **ggplot2** as package dependencies.

# MazamaSatelliteUtils 0.4.4

* Added `goesaodc_createMultiScanPoints()` function.
* Added `goesaodc_createSingleScanPoints()` function.
* Added `goesaodc_plotAODPoints()` function.

# MazamaSatelliteUtils 0.4.3

* Added `goesaodc_plotAveragePoints()` function.

# MazamaSatelliteUtils 0.4.2

* `endtime` params are now exclusive. The time they specify is the first time 
excluded from the time range.
* The `...` param in `goesaodc_areaPlot()` is now passed on correctly.
* `goesaodc_areaPlot()` now accepts standalone ncdf4 handles for the `nc` param.
* Added `dropNa` param to `goesaodc_createTibble()` and 
`goesaodc_createSpatialPoints()`.
* Added `baseUrl` param to `goesaodc_listDaytimeFiles()`.
* Removed `verbose` param from `goesaodc_createNativeGrid()`.
* Removed `verbose` from `goesaodc_createTibble()`.
* Updated `goesaodc_openFile()` to open multiple nc files at once.
* Added param validation to various functions.
* Improved verbosity of various `verbose` functions.
* Functions with default CONUS bboxes now use the `bbox_CONUS` package variable.
* Updated docs.
* Improved code style.

# MazamaSatelliteUtils 0.4.1

* Harmonized code and documentation style with other Mazama Science packages.

# MazamaSatelliteUtils 0.4.0

Version 0.4 is a large scale refactor of code with general improvements to bring
this package up to Mazama Science standards. (Version 0.3 was skipped
intentionally.)

* Fixed error in `bboxToVector()`.
* Fixed `bbox` used in the `goesaodc_areaPlot()` example.
* Updated documentation.
* Minor internal refactoring.
* Removed nonfunctional exploratory scripts and files.

# MazamaSatelliteUtils 0.2.13

* Updated documentation and vignettes.

# MazamaSatelliteUtils 0.2.12

* `goesaodc_createNativeGrid()` now properly reads and scales the raw data 
which are stored as `unsigned short int` values.
* New `goesaodc_areaPlot()` function creates nice plots of AOD.

# MazamaSatelliteUtils 0.2.11

* `goesaodc_downloadAOD()` and `goesaodc_downloadDaytimeAOD()` now return a 
vector of all relevant files found in `satelliteDataDir/` rather than just those 
that were downloaded.

# MazamaSatelliteUtils 0.2.10

* New `goesaodc_createNativeGrid()` function

# MazamaSatelliteUtils 0.2.9

* New `getDaylightHours()` function.
* More tests and improved examples.

# MazamaSatelliteUtils 0.2.8

* Added optional `endtime` argument to `goesaodc_listFiles()` and 
`goesaodc_downloadAOD()`.
* Various internal refactoring.
* Improved examples.

# MazamaSatelliteUtils 0.2.7

* Adding proper vectorization to `isDaylight()`.
* Added `goesaodc_listLocalFiles()` and `goesaodc_listRemoteFiles()`.

# MazamaSatelliteUtils 0.2.6

Added new `bboxToVector()` function to support the creation of bounnding box
extents in vector format from any type of bbox object passed in. This allowed
the removal of parameters `lonLo, lonHi, latLo latHi` from the following
functions:

* `goesaodc_createRaster()`
* `goesaodc_createDaytimeRasterStack()`
* `goesaodc_createHourlyRasterStack()`
* `goesaodc_createSpatialPoints()`

Various additional cleanup including harmonizing the use of parameters:

* `satID`
* `datetime`
* `jdate`

# MazamaSatelliteUtils 0.2.5

Added support for using the extents of rasters as the bbox to test whether 
a specified time falls whithin the daylight hours of the region. 

# MazamaSatelliteUtils 0.2.4

* Updated startdate code to use MazamaCoreUtils parseDatetime() to read the 
specified datetime from the user.  Also updated the jdate logic to work properly
 with full GOES .nc filename times.
* Implemented fullDay logic to allow the user to force a full day file listing 
and/or download, even if the startdate they specify includes hours (or minutes 
and seconds in a Julian date).

# MazamaSatelliteUtils 0.2.3

* New `installGoesGrids()` function creates lat-lon grids from the package
internal GOES-16 annd Goes-17 netcdf files.
* All code now expects to find GOES grids located in the directory specified
by `setSatelliteDataDir()`
* Improved documentation and examples.

# MazamaSatelliteUtils 0.2.2

Including two satellite images as raw data for testing.

# MazamaSatelliteUtils 0.2.1

Updated variable names to pass checks.

# MazamaSatelliteUtils 0.2.0

Lots of work testing and using the package to determine what new functionality
needs to be added. Improved documentation throughout including function docs
and vignettes.

Added functions:

* `goesaodc_createHourlyAverageRaster()`
* `raster_createLocationTimeseries()`
  

# MazamaSatelliteUtils 0.1.2

Standardized time-related parameters to all be "starttime", which emphesizes
that times refer to ABI scan start times.

Added unit testing.

# MazamaSatelliteUtils 0.1.1

New functions to download and work with GOES AOD data

Added functions:

* `goesaodc_createHourlyRasterStack()`
* `goesaodc_createRaster()`
* `goesaodc_createSpatialPoints()`
* `goesaodc_createTibble()`
* `goesaodc_downloadAOD()`
* `goesaodc_listFiles()`
* `goesaodc_openFile()`
* `goesaodc_plotSpatialPoints()`
* `goesaodc_getCoordBounds()`
* `goesaodc_getCoordGrid()`
* `goesaodc_getProjection()`
* `goesaodc_isGoesProjection()`
* `goesaodc_getStartString()`
* `getAttributes()`
* `isDaylight()`
* `getValue()`

# MazamaSatelliteUtils 0.1.0

* Initial package creation with functions ported from `report-nasa-maiac`.


