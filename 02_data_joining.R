#### SETUP ####
# Attach sf for its functions. Note: you will receive an error, this is something related to the package
library(sf)
# Attach the tidyverse for tidyr, dplyr, and ggplot functions
library(tidyverse)
# Attach lubridate to deal with dates
library(lubridate)
# Attach ggplot for graphing
library(ggplot2)
# call in the LDC from github
devtools::install_github("landscape-data-commons/trex", build_vignettes = TRUE)

# Attach trex for the LDC access
library(trex)


#### STEPS IN THIS CODE
# STEP 1: CONFIGURE THE GEODATE
# STEP 2: CONFIGURE LDC DATA
# STEP 3: FIND OVERLAP BETWEEN STATES AND LDC DATA


# STEP 1: CONFIGURE THE GEODATE
# Setting the path to the geodatabase 
gdb_path <- "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\SDS_casestudy\\SDS_casestudy\\JERStateMap_v1.gdb"

# List all layers in the geodatabase
st_layers(gdb_path)

# Read the specific layer from the geodatabase
gdb_layer <- st_read(gdb_path, layer = "JERStateMap_v1")


# STEP 2: CONFIGURE LDC DATA
# Import the headers info from the Landscape Data Commons API
headers_df <- trex::fetch_ldc(data_type = "header")

# Import the headers info from the Landscape Data Commons API
indicators_df <- trex::fetch_ldc(data_type = "indicators")


# STEP 3: COMBINING LDC AND JER
# Check for invalid geometries
invalid_geometries <- sum(!st_is_valid(gdb_layer))
if (invalid_geometries > 0) {
  cat("There are", invalid_geometries, "invalid geometries. Fixing them...\n")
  
  # Fix invalid geometries
  gdb_layer <- st_make_valid(gdb_layer)
}

# Reproject the geometries to match CRS
gdb_sf <- st_transform(gdb_layer, st_crs(headers_sf))

# Perform the spatial join
joined_sf <- st_join(x = gdb_sf, y = headers_sf)

# Combine the data frames and filter for rows visited in 2018
aero_state_2018_df <- LDC_JER_df %>%
  inner_join(indicators_df, by = "PrimaryKey") %>%  # Adjust the join type if needed
  filter(year(ymd_hms(DateVisited.x)) == 2018)      # Keep rows where the year is 2018

# Combine the data frames and filter for rows visited in 2018
aero_state_df <- LDC_JER_df %>%
  inner_join(indicators_df, by = "PrimaryKey")      # Keep rows where the year is 2018





