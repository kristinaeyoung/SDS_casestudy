#### SETUP ####
# Attach sf for its functions. Note: you will receive an error, this is something related to the package
library(sf)
# Attach the tidyverse for tidyr, dplyr, and ggplot functions
library(tidyverse)
# Attach lubridate to deal with dates
library(lubridate)
library(ggplot2)


#### STEPS IN THIS CODE
# STEP 1: CONFIGURE THE GEODATE
# STEP 2: EXAMINE THE GEODATABASE LAYERS
# STEP 3: MAKE MAPS OF THE GEODATABASE


# STEP 1: CONFIGURE THE GEODATE
# Setting the path to the geodatabase 
gdb_path <- "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\SDS_casestudy\\SDS_casestudy\\JERStateMap_v1.gdb"

# List all layers in the geodatabase
st_layers(gdb_path)

# Read the specific layer from the geodatabase
gdb_layer <- st_read(gdb_path, layer = "JERStateMap_v1")


# STEP 2: EXAMINE THE GEODATABASE LAYERS
# Print the structure of the layer to understand its contents
str(gdb_layer)

# Display the first few rows of the data
head(gdb_layer)

# Summarize the spatial and attribute data
summary(gdb_layer)


# STEP 3: MAKE MAPS OF THE GEODATABASE
# Basic map of the polygons, colored by 'soil_mu'
ggplot(data = gdb_layer) +
  geom_sf(aes(fill = soil_mu)) +  # Color by soil mapping unit
  theme_minimal() +
  labs(title = "Soil Mapping Units",
       fill = "Soil MU") +
  scale_fill_viridis_d()  # Use viridis color scale for discrete variables

# Map colored by MLRA (ecological regions)
ggplot(data = gdb_layer) +
  geom_sf(aes(fill = MLRA)) +  # Color by MLRA
  theme_minimal() +
  labs(title = "Major Land Resource Areas (MLRA)",
       fill = "MLRA") +
  scale_fill_viridis_d()  # Viridis color scale for categorical data

# Map colored by ecological state
ggplot(data = gdb_layer) +
  geom_sf(aes(fill = factor(state_code)), lwd = 0.2) +  # Convert state_code to factor
  theme_minimal() +
  labs(title = "State Codes",
       fill = "State Code") +  # Correct the legend label
  scale_fill_viridis_d()
