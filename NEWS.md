# MazamaSatelliteUtils 0.2.3

* New `installGoesGrids()` function creates lat-lon grids from the package
internal GOES-16 annd Goes-17 netcdf files.
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


