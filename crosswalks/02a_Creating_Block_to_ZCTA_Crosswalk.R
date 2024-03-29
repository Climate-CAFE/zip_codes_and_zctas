# This script is formatted to be run on a computing cluster via a bash script.
# For background information on bash scripting, see the documentation in the sub-directory
# called "bash_scripting" in the CAFE GitHub repository.
#
# The variables below are created within the bash script itself (see 02b* script) 
# and are read into this R script to subset the data to a smaller area of interest. 
# This enables the cluster to run multiple, simultaneous processes of the same 
# script on different locations, thus drastically reducing total wall-clock time.
#
# If you wish to use this script locally, i.e., without bash scripting, you can 
# lightly amend the code to directly assign the variables below. For example,
# if you want to run the code locally just for D.C., create a clone of the script
# and set stateindex <- 9 (D.C. is the 9th state or territory when ordered sequentially by FIPS).
#
# This script creates a crosswalk relationship file between census blocks (the 
# smallest administrative unit available in the US Census) and ZIP Code Tabulation
# Areas (ZCTAs). Any given census block is intended to be fully contained in only
# one ZCTA, though slight corrections to boundaries can occur between Decennial Census 
# years. This script does NOT account for these negligible cross-ZCTA areas and
# instead assigns blocks to ZCTAs based on block centroids. This script is useful
# for when you need to aggregate block-level information (such as temperature estimates
# derived in the tutorial "/population_weighting_raster_data/Population_Weighting_Raster_to_Census_Units.R")
# up to ZCTA resolution.
#
library("sf")         # Used for reading/write vector files and performing spatial analysis
library("tigris")     # Used to download census shapefiles; no API needed
library("doBy")       # Used to perform calculations on data by groups
library("tidycensus") # Used to download census data; API required (see below)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)  # S2 is for computing distances, areas, etc. on a SPHERE (using
                  # geographic coordinates, i.e., lat/lon in decimal-degrees); no need for this
                  # extra computational processing time if using PROJECTED coordinates,
                  # since these are already mapped to a flat surface. Here, all of the shapefiles
                  # are indeed in geographic coordinates, but we are only identifying
                  # intersections between blocks and ZCTAs in the same CRS. Since
                  # blocks are designed to nest within ZCTAs, any error introduced
                  # by this process on such small areas would be negligible.

# Check package version numbers
#
if (packageVersion("terra") < "1.5.34"    | packageVersion("sf") < "1.0.7"       | 
    packageVersion("tigris") < "2.0.4"    | packageVersion("doBy") < "4.6.19"    |
    packageVersion("tidyverse") < "1.3.1" ) {
  cat("WARNING: at least one package is outdated and may result in errors. \n") }

# Function for summing values that returns NA if all values are NA and NA values are removed
# PSA: sum(c(NA,NA,NA), na.rm = TRUE) yields "0" rather than NA. sumfun(c(NA,NA,NA)), below, returns NA.
#
sumfun <- function(x) { ifelse(all(is.na(x)), return(NA), return(sum(x, na.rm = TRUE))) }

# %%%%%%%%%%%%%%%%%%%%%%% USER-DEFINED PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#
input_data_dir <- "In_Dir/"
output_data_dir <- "Out_Dir/State_Level_Files/" # Enter the full pathway of the directory where your output data will be stored.

args <- commandArgs(trailingOnly = TRUE)  # These are arguments passed by the bash script
stateindex <- as.numeric(args[1])         # This is the index of the state/territory being processed (1-51 for 50 states + DC)

# Automated QC -- check to make sure the argument was passed successfully from the bash script
#
if (is.na(stateindex)) { cat("ERROR: your state FIPS index was NOT successfully passed by the bash script \n") }

year <- 2020  # For the purpose of this tutorial, we are calculating only 2020 data,
              # but this can be run for other years. Note that syntax and/or
              # variable names can change between years and Decennial Censuses.
              # For this particular application, it is recommended to use
              # Decennial Census years (2010, 2020, etc.).
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Get the state FIPS code based on the index from the bash script
#
states <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
            "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
            "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")
stateFIPS <- states[stateindex] # "stateindex" comes from the bash script / compiler. Set b <- 11 for D.C. if testing code locally.


# %%%%%%%%%%%%%% STEP 1. READ IN CENSUS BLOCK AND ZCTA SHAPEFILES %%%%%%%%%%% #
#
# Read in the block-level shapefile
#
blocks <- st_read(paste0(input_data_dir, "Blocks_", year, "_", stateFIPS, ".gpkg"))

# The ZCTA shapefile is a large national file -- we do not need to waste memory
# by reading in the entire thing; we can read in just the features that are relevant
# to the state being processed.
#
# Get the bounding box of the blocks features
#
bbox <- st_bbox(blocks)

# Give a bit of a buffer around the AOI to ensure all polygons are kept
#
bbox[["xmin"]] <- bbox[["xmin"]] - 0.0625
bbox[["ymin"]] <- bbox[["ymin"]] - 0.0625
bbox[["xmax"]] <- bbox[["xmax"]] + 0.0625
bbox[["ymax"]] <- bbox[["ymax"]] + 0.0625

# Create a polygon representing this bounding box and format as WKT
#
extent_poly <- st_as_text(st_as_sfc(bbox, crs = st_crs(blocks)))

# Use WKT to filter the data being loaded from the GeoPackage file.
# H/T Dennis Milechin and Zach Popp
#
zctas <- st_read(paste0(input_data_dir, "ZCTA_", year, "_US.gpkg"),
                 wkt_filter = extent_poly)

# Automated QC check: confirm both shapefiles have the same CRS
#
if (!isTRUE(all.equal(st_crs(blocks), st_crs(zctas)))) {
  cat("WARNING: CRS's don't match. Transforming... \n")  
  blocks <- st_transform(blocks, crs = st_crs(zctas))
  if (!isTRUE(all.equal(st_crs(blocks), st_crs(zctas)))) { 
    cat("..... ERROR: CRS's still do not match \n") } else { cat("..... :) fixed! CRS's now match \n") }
} else { cat(":) CRS's match \n") }

# Fix any potential geometry errors
#
blocks <- st_make_valid(blocks) 
zctas <- st_make_valid(zctas)

#
# %%%%%%%%%% STEP 2. CONVERT BLOCKS TO POINTS AND INTERSECT WITH ZCTAS %%%%%%% #
#
block_centroids <- st_point_on_surface(blocks)
block_zctas <- st_intersection(block_centroids, zctas)

block_geoid <- names(block_centroids)[grep("^GEOID", names(block_centroids))]
block_pop_var <- names(block_centroids)[grep("^POP", names(block_centroids))]
block_housing_var <- names(block_centroids)[grep("^HOUSING", names(block_centroids))]
land_area_var <- names(block_centroids)[grep("^ALAND", names(block_centroids))]
zcta_geoid <- names(block_zctas)[grep("^ZCTA5", names(block_zctas))]

# Automated QC check -- confirm all block centroids have been linked to a ZCTA (if available)
# NOTE: not every block will necessarily have an accompanying ZCTA -- ZCTA's have large 
#       gaps in places where there is no mail delivery, such as nature preserves.
#       Here, we're checking to ensure that the only unassigned blocks are those
#       that: (1) are all-water tracts, (2) have population <= 10, and/or (3) have
#       5 or fewer housing units. These are all indicators of blocks that we would
#       not expect to have an accompanying ZCTA. 
#
unassigned_blocks <- block_centroids[which(!block_centroids[[block_geoid]] %in% block_zctas[[block_geoid]]),]
populated <- which(unassigned_blocks[[block_pop_var]] > 10 & 
                     unassigned_blocks[[land_area_var]] > 0 &
                     unassigned_blocks[[block_housing_var]] > 5)
if (dim(block_zctas)[1] != dim(block_centroids)[1] & length(populated) > 10) {
  
  cat("ERROR:", length(populated), "populated blocks do not have ZCTA assignments. Details below: \n") 
  print(as.data.frame(unassigned_blocks[populated,c(block_geoid, land_area_var, block_pop_var, block_housing_var)]))
  
} else if (dim(block_zctas)[1] != dim(block_centroids)[1] & length(populated) > 0) {
  
  cat("WARNING:", length(populated), "populated blocks do not have ZCTA assignments. Details below: \n") 
  print(as.data.frame(unassigned_blocks[populated,c(block_geoid, land_area_var, block_pop_var, block_housing_var)]))
  
} else { cat(":) all populated blocks have been linked to a ZCTA \n") }

# Remove the extraneous columns; we only need block GEOID, ZCTA GEOID, and population (if available)
# n.b., some census block shapefiles include populations but not others. The code
# here assumes that the shapefile does not include population. The name of the GEOID
# will change depending on the census year; confirm that block_geoid correctly
# identifies it. Modify as needed. Both the blocks and ZCTA shapefiles may have
# variables called GEOID; the block one will come first if intersection is done
# with block first.
#
block_zctas <- block_zctas[,c(block_geoid, zcta_geoid)]

# %%%%%%%%%%%%%%%%% STEP 3. READ IN & MERGE POPULATION COUNTS %%%%%%%%%%%%%%% #
# 
# NOTE: if you didn't download your population data following script 01, then you
#       will need to edit this to read in the correct filename of your population data
#
pop_var_name <- "P1_001N"
geography <- "block"
decennial_year <- ifelse(year %in% 2000:2009, 2000,
                         ifelse(year %in% 2010:2019, 2010,
                                ifelse(year %in% 2020:2029, 2020, NA)))
blockpop <- readRDS(paste0(input_data_dir, "Census_", pop_var_name, "_", geography, "_", decennial_year, "_", stateFIPS, ".Rds"))

# Automated QC check -- confirm data is total population
#
if (length(which(blockpop$variable != pop_var_name)) > 0) {
  cat("ERROR: Not all variables are", pop_var_name, " \n") } else { cat(":) all variables are", pop_var_name, " \n") }

pop_var_name <- paste0("Pop_", decennial_year)
names(blockpop)[grep("^value$", names(blockpop), ignore.case = TRUE)] <- pop_var_name
blockpop[,c("NAME", "variable")] <- NULL

# Get the GEOID for the block shapefile and for the block populations file, which may 
# change depending on the year of Census data
#
GEOID_shapefile <- names(block_zctas)[grep("^GEOID", names(block_zctas), ignore.case = TRUE)]
GEOID_popfile <- names(blockpop)[grep("^GEOID", names(blockpop), ignore.case = TRUE)]

# Merge the population data with the blocks_zcta shapefile
#
block_zctas <- merge(block_zctas, blockpop, by.x = GEOID_shapefile, by.y = GEOID_popfile, all.x = TRUE)
num_missing <- length(which(is.na(blocks[[pop_var_name]])))

if (num_missing > 0) {
  cat("WARNING:", num_missing, "blocks with missing population \n") 
} else { cat(":) no blocks with missing population \n") }

#
# %%%%%%%%%%%% STEP 4. CREATE SPATIAL WEIGHTS FOR FINAL CROSSWALK %%%%%%%%%%%% #
#
# Get the total population of ZCTAs based on the blocks that are within each ZCTA
#
# Convert block_zctas sf object to data frame
#
block_zctas <- as.data.frame(block_zctas)
block_zctas$geometry <- NULL

popvar <- names(block_zctas)[grep("^Pop", names(block_zctas), ignore.case = TRUE)]
eqn <- as.formula(paste0(popvar, " ~ ", zcta_geoid))
zcta_pop <- doBy::summaryBy(eqn, data = block_zctas, FUN = sumfun)
names(zcta_pop)[grep("^Pop", names(zcta_pop), ignore.case = TRUE)] <- "Pop_ZCTA"

block_zctas <- merge(block_zctas, zcta_pop, by = zcta_geoid, all.x = TRUE)

block_zctas$Spatial_Weight <- block_zctas[[popvar]] / block_zctas[["Pop_ZCTA"]]

# Automated QC -- confirm spatial weights add to 1 within each ZCTA
#
eqn <- as.formula(paste0("Spatial_Weight ~ ", zcta_geoid))
qc_check <- summaryBy(eqn, data = block_zctas, FUN = sumfun)

if (length(which(round(qc_check$Spatial_Weight.sumfun, 4) != 1)) > 0) {
  cat("ERROR: spatial weights do not all sum to 1 \n") } else { cat(":) all spatial weights sum to 1 \n") }

# Save the crosswalk file
#
final <- block_zctas[,c(block_geoid, zcta_geoid, popvar, "Pop_ZCTA", "Spatial_Weight")]
saveRDS(final, paste0(output_data_dir, "Block_to_ZCTA_", year, "_", stateFIPS, ".Rds"))
