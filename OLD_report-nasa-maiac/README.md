# NASA MAIAC

Compare the new NASA MAIAC product with Bluesky and monitoring data.

This is a project for Susan O'Neill at the USFS Pacific Wildland Fire Science Lab. It contains explanatory and exploratory reports for comparing MAIAC AOD with PM2.5 monitoring data and Blueksy modeling data, and tools for working with and comparing these data, including an R package `AirfireNasaMaiac` designed for working with MAIAC data from the North America tiles.  

The package and associated example reports and notebooks form a collection which should provide powerful tools for working with MAIAC data. The reports in the `localReports/` directory contain an overview of how data is loaded and some examples of what can be done with the tools provided. Reports are presented as .html and .Rmd files. The .html files can be opened in a browser and contain images, plots, and output from analyses. The .Rmd files contain the code used to generate the output. The reports are numbered to follow a logical trajectory. 

The directory 'localNotebooks' contains more loosely-formatted notebooks with more examples and code and contain a more in-depth exploration of the data and an archive of the processes used to create the reports. 

## Prerequisites

This package and the code in the `R` directory depends on the following R packages:

`lubridate`  
`ncdf4`  
`raster`  
`stringr`  
`xml2`  

Furthermore, the .Rmd reports use the following additional R packages:

`knitr`  
`pscl`  
`PWFSLSmoke`  
`PWFSLSmokeModeling`  
`rmarkdown`  

The netCDF library must be installed on your machine. It can be manually downloaded from [UniData](https://www.unidata.ucar.edu/downloads/netcdf/index.jsp) or through a package management system like MacPorts or Homebrew.

You must also have a platform-appropriate hdf to netcdf converter locally available. The version for Mac OS 10.12 (Sierra) is saved in the executables/ directory. Binaries for other platforms can be found at [http://www.hdfeos.org/software/h4cflib.php](http://www.hdfeos.org/software/h4cflib.php). Use the `converterPath` argument in `maiac_2nc4()` and `maiac_loadRaster()` to specify where it is saved. 

## Installing the package

From the package directory:  
`devtools::install(getwd())`

Alternatively, from RStudio: go to the "Build" tab and click "Install and Restart". 



## Directory Structure

```
nasa-maiac
├── DESCRIPTION
├── NAMESPACE
├── R/
├── README.md
├── R_old/
├── localImages/
├── executables/
├── localData/
├── localData_old/
├── localExamples/
├── localNotebooks/
├── localReports/
├── man
└── nasa-maiac.Rproj
```

The minimum files required to build and use the package are: 

```
nasa-maiac
├── DESCRIPTION
├── NAMESPACE
├── R/
├── executables/
├── man
```

The folders with the suffix `_old` contain code and data that may not be relevant to the current project, but are kept for archival and accountablity purposes.  
   

* `R/`  
   Functions can be found in the `R` folder. Some documentation can be found in each script. Source these files or build the package to load the functions into the environment and use them. For an overview of what can be accomplished with these functions and the raster functionalities in R, see the `localReports` folder. 

* `localReports/`   
   The `localReports` directory contains several 'reports', formatted to give an overview of functionality. View the .html files to see the final report, and the .Rmd files to see the R code that was used to create the analysis and plots. 
   
* `localNotebooks/`  
   The `localNotebooks` folder contains a series of .Rmd notebooks. The titles are formatted maiac_\<date\>_\<title\>. View these notebooks for some more details and additional plots not included in the reports. 
   
* `localData/`  
   The `localData` directory includes some .Rdata files, including preformatted rasters and dataframes. Functions to create these data are in the `R` directory. 
   
 * `localImages/`  
   This directory contains some reference files: `MAIACData_UsrGd.pdf` (the MAIAC users guide), `MAIAC_Tiles_NorthAmerica.jpg` (an image showing the location and names of North America MAIAC tiles), `confusion_matrix.png` (image detailing the format of a confusion matrix). 

### Working with MAIAC data

The files in the .R directory are designed specifically for working with NASA MAIAC North America data, found [here](https://portal.nccs.nasa.gov/datashare/maiac/DataRelease/NorthAmerica_2000-2016/). Some documentation on this data can be found in the users guide ('MAIACData_UsrGd.pdf' -- found in the top directory level) from NASA. 

The top-level function is `maiac_loadRaster`. This will:  

* download the .hdf file from NASA (if not found locally)  
* convert the .hdf file to .nc (netcdf)  
* pull out some spatial metadata  
* load the desired variable as a Raster* object using the `raster` package  

This calls a number of lower-level functions which are also saved in the R directory which must be available in the R environment to run this function.

Once you have the MAIAC data loaded as a Raster* object, you can use the impressive functionality of the R `raster` package to manipulate it, allowing you to easily do things like reproject, pull out values, aggregate, resample, and more with just one line of code. 

