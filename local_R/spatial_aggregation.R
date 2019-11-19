#  So i've managed to create a list that contains a tibble for every timestep that was given as an input file.
# Note that the coordinate values and counts for each tibble don't match.

# names(filtered_tbl)
# [1] "201910271201" "201910271416"

# > head(filtered_tbl)
# $`201910271201`
# A tibble: 1,920 x 4
#     lon   lat     AOD   DQF
#   <dbl> <dbl>   <dbl> <dbl>
# 1 -125.  39.2 0.00972     2
# 2 -125.  39.2 0.0116      2
# 3 -125.  39.2 0.0171      2
# 4 -125.  39.2 0.0126      2
# 5 -125.  39.2 0.0102      2
# 6 -125.  39.2 0.0130      2
# 7 -125.  39.2 0.0124      2
# 8 -125.  39.1 0.0107      2
# 9 -125.  39.1 0.0187      2
#10 -125.  39.1 0.0141      2
# ... with 1,910 more rows

#$`201910271416`
# A tibble: 1,911 x 4
#     lon   lat    AOD   DQF
#   <dbl> <dbl>  <dbl> <dbl>
# 1 -124.  39.0 0.0691     0
# 2 -124.  39.0 0.0548     0
# 3 -124.  39.0 0.0619     0
# 4 -124.  39.0 0.0476     0
# 5 -124.  39.0 0.0375     0
# 6 -124.  39.0 0.0704     0
# 7 -124.  39.0 0.0332     0
# 8 -124.  39.0 0.0503     1
# 9 -124.  39.0 0.0597     1
#10 -124.  39.0 0.0764     1
# ... with 1,901 more rows

# The code I used to generate it looks like this:

library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

#kincade_bbox <- c(-124, -120, 36, 39) # original values
kincade_bbox <- c(-124, -123, 38, 39) # small area
 
my_files <- c("OR_ABI-L2-AODC-M6_G16_s20193001901344_e20193001904117_c20193001907158.nc",
 "OR_ABI-L2-AODC-M6_G17_s20193002116196_e20193002118569_c20193002121014.nc")

filtered_tbl <- goesaodc_createNativeGrid(
  my_files,
  bbox = kincade_bbox)

print(head(filtered_tbl))

# MINIMUM LON/LAT COORDS DON'T MATCH BETWEEN FILES
for (timestamp in names(filtered_tbl)) {
  data <- filtered_tbl[[timestamp]]
  print(timestamp)
  min_lon <- min(data$lon)
  min_lat <- min(data$lat)
  print(sprintf("%f,%f", min_lon, min_lat))
}

# ONE WAY WE COULD DO IT...

# SOME SAMPLE DATA
#     layer_1      #
#  lon   lat    AOD
# -124.0  39.0   18.5
# -124.0  39.1   18.5
# -125.0  39.1   18.5
# -125.0  39.2   18.5
#------------------#
#      layer_2     #
# -124.0  39.0   21.5
# -124.0  39.1   21.5
# -125.0  39.3   21.5
# -125.0  39.1   21.5
####################
#library(dplyr)

varlist_1 <- list()
varlist_2 <- list()
varlist_3 <- list()

# LIST 1
varlist_1[["lon"]] <- c(-124.0, -124.0, -125.0, -125.0)
varlist_1[["lat"]] <- c(39.0, 39.1, 39.1, 39.2)
varlist_1[["AOD"]] <- c(18.5, 18.5, 18.5, 18.5)

tbl_1 <- tibble::as_tibble(varlist_1)
print(tbl_1)

# LIST 2
varlist_2[["lon"]] <-  c(-124.0, -124.0, -125.0, -125.0)
varlist_2[["lat"]] <- c(39.0, 39.1, 39.3, 39.1)
varlist_2[["AOD"]] <- c(21.5, 21.5, 21.5, 21.5)

tbl_2 <- tibble::as_tibble(varlist_2)
print(tbl_2)

# LIST 3,  COMBINED THEM ALL INTO THE SAME DATA SET
varlist_3[["lon"]] <- c(-124.0, -124.0, -125.0, -125.0,-124.0, -124.0, -125.0, -125.0)
varlist_3[["lat"]] <- c(39.0, 39.1, 39.1, 39.2, 39.0, 39.1, 39.3, 39.1)
varlist_3[["AOD"]] <- c(18.5, 18.5, 18.5, 18.5, 21.5, 21.5, 21.5, 21.5)

tbl_3 <- tibble::as_tibble(varlist_3)

# GROUP BY AND SUMMARIZE ON THE COORDINATES
output <- tbl_3 %>%
  dplyr::group_by(lon, lat) %>%
  dplyr::summarise(mean = mean(AOD))
  
print(output)

# ---- IMPLEMENT JON'S AVERAGING ACROSS LISTS FUNCTIONALITY

# 1st ADD DQF VALUES TO MIMIC REAL DATA

dqf_vals <- c(2, 2, 1, 2)

varlist_1[["DQF"]] <- dqf_vals
varlist_2[["DQF"]] <- dqf_vals

label_1 <- "201910271201"
label_2 <- "201910271416"

nativeGridList <- list()
nativeGridList[[label_1]] <- varlist_1
nativeGridList[[label_2]] <- varlist_2

print(nativeGridList)

# NOW FOR THE AVERAGING
# Create a list containing only 2-D AOD arrays
AODList <- nativeGridList %>% purrr::map(~ .x$AOD)

# Create a 3-D AOD array
stackedNativeArray <- abind::abind(AODList, rev.along = 0)

# Calculate the mean at every x-y location
AOD_mean <- apply(stackedNativeArray, c(1,2), mean, na.rm = TRUE)

# Have a look
image(AOD_mean)
