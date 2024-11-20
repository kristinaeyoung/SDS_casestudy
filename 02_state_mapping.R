#### SETUP ####
# Load necessary libraries
library(sf)          # For handling spatial data
library(tidyverse)   # For data manipulation and visualization
library(lubridate)   # For date handling
library(ggplot2)     # For additional plotting functionality

# Set the path to the geodatabase
sf_path <- "C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\SDS_casestudy\\SDS_casestudy\\2021_JornadaStateMap.shp"

# List all layers in the geodatabase
st_layers(sf_path)

#### DATA EXPLORATION ####
# Explore the geodatabase layer
str(sf_layer)        # Check the structure of the data
head(sf_layer)       # Display the first few rows
summary(sf_layer)    # Summarize spatial and attribute data

#### JOIN ECOLOGICAL STATE CODES ####
# Read the CSV containing ecological state codes
code <- read.csv("statecode_state.csv")

# Join the code data with the geodatabase layer
sflayer_joined <- sf_layer %>%
  left_join(code, by = c("esite", "state_code"))

# Verify join data
head(sf_layer_joined)

#### VISUALIZE ECOLOGICAL SITES ####
# Map all ecological sites
ggplot(data = sf_layer_joined) +
  geom_sf(aes(fill = factor(esite)), lwd = 0.2) +
  theme_minimal() +
  labs(title = "Ecological Sites",
       fill = "Ecological Site") +
  scale_fill_viridis_d()

#### SUBSET AND VISUALIZE SPECIFIC SITES ####
# Filter the layer for "Sandy" sites
subset_sandy <- sf_layer_joined %>%
  filter(esite == "Sandy")

# Check unique ecological sites in the subset
unique(subset_sandy$esite)

# Ensure 'dominant_state_label' is a factor for plotting
subset_sandy$dominant_state_label <- as.factor(subset_sandy$dominant_state_label)

# Map the subsetted data by ecological state
ggplot(data = subset_sandy) +
  geom_sf(aes(fill = dominant_state_label), lwd = 0.2) +
  theme_minimal() +
  labs(title = "Ecological State - Sandy Sites",
       fill = "Dominant State") +
  scale_fill_viridis_d()

# Filter the layer for "Gravelly" sites
subset_gravel <- sf_layer_joined %>%
  filter(esite == "Gravelly")

# Check unique ecological sites in the subset
unique(subset_gravel$esite)

# Ensure 'dominant_state_label' is a factor for plotting
subset_gravel$dominant_state_label <- as.factor(subset_gravel$dominant_state_label)

# Map the subsetted data by ecological state
ggplot(data = subset_gravel) +
  geom_sf(aes(fill = dominant_state_label), lwd = 0.2) +
  theme_minimal() +
  labs(title = "Ecological State - Gravelly Sites",
       fill = "Dominant State") +
  scale_fill_viridis_d()


# Get unique values for both columns
unique_labels <- unique(subset_gravel$dominant_state_label)
unique_numbers <- unique(subset_gravel$dominant_state_number)

# Create a data frame from the unique values
unique_table <- data.frame(
  Dominant_State_Label = unique_labels,
  Dominant_State_Number = unique_numbers
)

# Display the table
print(unique_table)
