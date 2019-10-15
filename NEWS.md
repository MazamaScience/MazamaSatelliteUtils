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


