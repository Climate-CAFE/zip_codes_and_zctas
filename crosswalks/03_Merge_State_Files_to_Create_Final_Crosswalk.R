# This script takes all of the state-level, block-to-ZCTA crosswalks created in 
# script 02 and combines them into a single nationwide crosswalk. It also checks
# the error logs from the distributed computing to ensure that no errors were thrown
#
year <- 2020

input_data_dir <- "In_Dir/"
output_data_dir <- "Out_Dir/State_Level_Files/"
final_output_data_dir <- "Out_Dir/"

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# %%%%%%%%%%% CHECK THE LOGS FOR ERROR MESSAGES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# Identify all of the log files from the days you have processed
#
logs <- list.files(pattern = "Block2ZCTA\\.o[0-9]*")

if (length(logs) != 51) { cat("ERROR: number of logs does not equal 51 \n") }

# Check the logs - if the log contains the word ERROR then something went wrong
#
for (i in 1:length(logs)) {
  if (!identical(character(0), grep("ERROR", readLines(logs[i]), value = TRUE, ignore.case = FALSE))) {
    print(paste0("ERROR: ", logs[i])) } else { print(paste0(":) ", logs[i]))}
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# %%%%%%%%%%%%%%%%%%%%%% MERGE THE OUTPUT FILES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

state_files <- paste0(output_data_dir, list.files(output_data_dir, pattern = paste0("Block_to_ZCTA_", year, ".*.Rds")))

# Automated QC check -- ensure 51 files are present
#
if (length(state_files) != 51) { cat("ERROR: wrong number of files \n") } else { cat(":) 51 files present \n") }

national_xwalk <- do.call(rbind, lapply(state_files, readRDS))

# Get applicable variables
#
block_geoid <- names(national_xwalk)[grep("^GEOID", names(national_xwalk))]
zcta_geoid <- names(national_xwalk)[grep("^ZCTA5", names(national_xwalk))]

# Automated QC check -- confirm that there are no double-counted blocks & that all ZCTAs are present
#
if (length(unique(national_xwalk[[geoid]])) != dim(national_xwalk)[1]) {
  cat("ERROR: duplicate block GEOIDs! \n") } else { cat(":) no duplicate block GEOIDs \n") }

zctas <- st_read(paste0(input_data_dir, "ZCTA_", year, "_US.gpkg"))
zcta_id <- names(zctas)[grep("^ZCTA5", names(zctas))]
zcta_lat_var <- names(zctas)[grep("^INTPTLAT", names(zctas))]

# Subset to 50 states + DC
#
zctas <- zctas[which(substr(zctas[[zcta_id]], 1, 2) != "00" &
                       as.numeric(zctas[[zcta_lat_var]]) > 18.5),]

all_zctas <- unique(zctas[[zcta_id]])

if (length(which( !(all_zctas %in% national_xwalk[[zcta_geoid]]) )) > 0) {
  cat("ERROR: some ZCTAs are missing from the national crosswalk! \n") 
} else { cat(":) all ZCTAs are present in the national crosswalk \n") }

saveRDS(national_xwalk, paste0(final_output_data_dir, "Block_to_ZCTA_", year, "_US.Rds"))
