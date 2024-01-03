# This script is formatted to be run on a computing cluster via a bash script.
# For more information on bash scripting, see the documentation in the sub-directory
# called "bash_scripting"
#
# The variables below are created within the bash script and are used in this R
# script to subset the data to a smaller area of interest. This enables the cluster
# to run multiple, simultaneous processes of the same script on different locations,
# thus drastically reducing total wall-clock time.
#
# If you wish to use this script locally, i.e., without bash scripting, you can 
# lightly amend the code to directly assign the variables below. For example,
# if you want to run the code locally just for D.C., create a clone of the script
# and set b <- 11 (D.C. is the 11th state or territory when ordered sequentially by FIPS).


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
library("sf")     # For vector data
library("plyr")
library("tigris") # For downloading census shapefiles
library("doBy")
library("tidyverse")
library("tidycensus")
library("lwgeom")
sf_use_s2(FALSE)  # S2 is for computing distances, areas, etc. on a SPHERE (using
                  # geographic coordinates, i.e., lat/lon in decimal-degrees); no need for this
                  # extra computational processing time if using PROJECTED coordinates,
                  # since these are already mapped to a flat surface. Here, all of the shapefiles
                  # are indeed in geographic coordinates, but the scale of areas we are 
                  # interested in is very small, and hence the error introduced by 
                  # ignoring the Earth's curvature over these tiny areas is negligible and
                  # a reasonable trade off given the dramatic reduction in processing time. Moreover,
                  # the areas we calculate are not an integral part of the process
                  # and any error in that step would not materially impact the final output
options(tigris_use_cache = TRUE)

# Check package version numbers
#
if (packageVersion("terra") < "1.5.34"   | packageVersion("sf") < "1.0.7" | 
    packageVersion("plyr")  < "1.8.7"    | packageVersion("tigris") < "2.0.4" |
    packageVersion("doBy")  < "4.6.19"   | packageVersion("tidyverse") < "1.3.1" |
    packageVersion("tidycensus") < "1.5" | packageVersion("lwgeom") < "0.2.8") {
  cat("WARNING: packages are outdated and may result in errors. \n") }

# User-defined function for summing values; returns NA if all values are NA
#
sumfun <- function(x) { ifelse(all(is.na(x)), return(NA), return(sum(x, na.rm = TRUE))) }

# %%%%%%%%%%%%%%%%%%%%%%% USER-DEFINED PARAMETERS %%%%%%%%%%%%%%%%%%%%%%%%%%%% #

census_api_key("")     # Obtain an API key here: https://www.census.gov/data/developers.html
                       # NOTE: You should NOT save your API key within your code

output_data_dir <- "Out_Dir/State_Level_Files/" # Enter the full pathway of the directory where your output data will be stored.

# !!!!! TEMPORARY !!!!!: add here the list of all state FIPS for 50 states + DC
states <- c()

stateFIPS <- states[b] # "b" comes from the bash script / compiler. Set b <- 11 for D.C. if testing code locally.

year <- 2020           # For the purpose of this tutorial, we are calculating only 2020 data,
                       # but this can be run for other years. Note that syntax and/or
                       # variable names can change between years and Decennial Censuses.
                       # For this particular application, it is recommended to use
                       # Decennial Census years (2010, 2020, etc.).
#
# %%%%%%%%%%%%%% STEP 1. DOWNLOAD CENSUS BLOCK AND ZCTA SHAPEFILES %%%%%%%%%%% #
#
blocks <- tigris::blocks(state = stateFIPS, year = year)
zctas <- tigris::zctas(year = year) # Cannot download ZCTAs by state; large file
                                    # If running as bash script across all 50 states + DC,
                                    # consider saving the nationwide ZCTA
                                    # shapefile locally to save time 
                                    # and bandwidth instead of downloading 
                                    # the same massive file 51 times

# Automated QC check: confirm both shapefiles have the same CRS
#
if (!isTRUE(all.equal(st_crs(blocks), st_crs(zctas)))) {
  cat("ERROR: CRS's don't match \n")  } else { cat(":) CRS's match \n") }

# Subset ZCTAs to the state of interest so processing time is cut down
#
bbox <- st_bbox(blocks)

# Give a bit of a buffer around the AOI to ensure all polygons are kept
#
bbox[["xmin"]] <- bbox[["xmin"]] - 0.0625
bbox[["ymin"]] <- bbox[["ymin"]] - 0.0625
bbox[["xmax"]] <- bbox[["xmax"]] + 0.0625
bbox[["ymax"]] <- bbox[["ymax"]] + 0.0625

zctas_state <- st_crop(zctas, bbox)

# Fix any potential geometry errors
#
blocks <- st_make_valid(blocks) 
zctas_state <- st_make_valid(zctas_state)

#
# %%%%%%%%%% STEP 2. CONVERT BLOCKS TO POINTS AND INTERSECT WITH ZCTAS %%%%%%% #
#
block_centroids <- st_point_on_surface(blocks)

block_zctas <- st_intersection(block_centroids, zctas_state)

# Automated QC check -- confirm all block centroids have been linked to a ZCTA (if available)
#
if (dim(block_zctas)[1] != dim(block_centroids)[1]) {
  cat("ERROR: incorrect ZCTA assignments for blocks") } else { cat(":) all blocks linked with ZCTA") }

# Remove the extraneous columns; we only need block GEOID, ZCTA GEOID, and population (if available)
# n.b., some census block shapefiles include populations but not others. The code
# here assumes that the shapefile does not include population. The name of the GEOID
# will change depending on the census year; confirm that block_geoid correctly
# identifies it. Modify as needed. Both the blocks and ZCTA shapefiles may have
# variables called GEOID; the block one will come first if intersection is done
# with block first.
#
block_geoid <- names(block_zctas)[grep("^GEOID", names(block_zctas), ignore.case = TRUE)[1]]
zcta_geoid <- names(block_zctas)[grep("^ZCTA", names(block_zctas), ignore.case = TRUE)]

block_zctas <- block_zctas[,c(block_geoid, zcta_geoid)]

# %%%%%%%%%%%%%%%%% STEP 3. DOWNLOAD & MERGE POPULATION COUNTS %%%%%%%%%%%%%%% #
#
# Obtain block-level population data. Note that the Census Bureau only releases
# population counts at the block level in the Decennial Census (2000, 2010, 2020, etc.).
# Note also that block identifiers (i.e., GEOID or FIPS code) can change over time,
# even between Decennial Censuses -- for example, a county may merge with an
# adjacent county and the Census Bureau will have to adjust the GEOIDs for the
# affected Census units within the relevant counties.
#
# You can download Census data in a variety of ways. Here, we are using the
# tidycensus package, which uses the Census Bureau's API. You must first obtain 
# an API key. Visit https://www.census.gov/data/developers.html and select the
# large "Request a Key" image on the left side of the screen to request an API key.
# Enter your API key at the top of the script under "USER-DEFINED PARAMETERS";
# it is assigned via the census_api_key() function.

# Block populations must be queried for each individual county. Identify all of the
# counties in the state being processed and then loop through them.
#
counties <- tigris::counties(state = stateFIPS, year = year, cb = TRUE)
counties <- unique(counties[[grep("^COUNTYFP", names(counties), ignore.case = TRUE)]])

# Block populations are only available for Decennial Censuses
#
decennial_year <- ifelse(year %in% 2000:2009, 2000,
                         ifelse(year %in% 2010:2019, 2010,
                                ifelse(year %in% 2020:2029, 2020, NA)))

for (i in 1:length(counties)) {
  
  blockpop_bycounty <- get_decennial(geography = "block",
                                     variables = "P1_001N", # Use load_variables("pl", year = 2020) to see available vars
                                     year = decennial_year,
                                     state = stateFIPS,
                                     county = counties[i],
                                     sumfile = "pl")
  if (i == 1) { blockpop <- blockpop_bycounty; next }
  blockpop <- rbind(blockpop, blockpop_bycounty)
}

# Automated QC check -- confirm data is total population
#
if (length(which(blockpop$variable != "P1_001N")) > 0) {
  cat("ERROR: Not all pop. vars. \n") } else { cat(":) all pop. vars. \n") }

pop_var_name <- paste0("Pop_", year)
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
zcta_pop <- summaryBy(eqn, data = block_zctas, FUN = sumfun)
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
