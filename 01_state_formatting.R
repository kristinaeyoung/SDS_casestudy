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
# devtools::install_github("landscape-data-commons/trex", build_vignettes = TRUE)

# Attach trex for the LDC access
library(trex)

# Setting the path to the data
ecosite <- read.csv("C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\SDS_casestudy\\SDS_casestudy\\EcoState_MLRA42.csv")

statekey <- read.csv("C:\\Users\\Kristina\\OneDrive - New Mexico State University\\Desktop\\GIT REPOs\\SDS_casestudy\\SDS_casestudy\\StateKey_StateNumbers_StateNames.csv")

statekey <- statekey %>%
  rename(EcologicalState = StateNumber)

# Perform the join based on two columns
joined_data <- ecosite %>%
  inner_join(statekey, by = c("EcologicalSiteId", "EcologicalState"))

# View the resulting data frame
head(joined_data)

# CONFIGURE LDC DATA
# Import the headers info from the Landscape Data Commons API
# headers_df <- trex::fetch_ldc(data_type = "header")

# Import the headers info from the Landscape Data Commons API
indicators_df <- trex::fetch_ldc(data_type = "indicators")

# Perform the join based on two columns
aero_data <- joined_data %>%
  inner_join(indicators_df, by = c("PrimaryKey"))


# Looking at the ecological sites of interest
filtered_aero_data <- aero_data %>%
  filter(SiteName %in% c("Sandy", "Gravelly")) %>%
  mutate(SiteName = as.factor(SiteName))

filtered_aero_data <- filtered_aero_data %>%
  mutate(
    updated_ES = case_when(
      # Gravelly mappings
      grepl("Gravelly", SiteName) & EcologicalState == 2.1 ~ 5,
      grepl("Gravelly", SiteName) & EcologicalState == 2.2 ~ 5,
      grepl("Gravelly", SiteName) & EcologicalState == 3.1 ~ 6,
      grepl("Gravelly", SiteName) & EcologicalState == 4.1 ~ 6,
      grepl("Gravelly", SiteName) & EcologicalState == 5.1 ~ 7,
      
      # Sandy mappings
      grepl("Sandy", SiteName) & EcologicalState == 1.1 ~ 1,
      grepl("Sandy", SiteName) & EcologicalState == 1.2 ~ 1,
      grepl("Sandy", SiteName) & EcologicalState == 2.1 ~ 2,
      grepl("Sandy", SiteName) & EcologicalState == 2.2 ~ 2,
      grepl("Sandy", SiteName) & EcologicalState == 3.1 ~ 4,
      grepl("Sandy", SiteName) & EcologicalState == 3.2 ~ 4,
      grepl("Sandy", SiteName) & EcologicalState == 4.1 ~ 6,
      grepl("Sandy", SiteName) & EcologicalState == 5.1 ~ 9,
      TRUE ~ NA_real_
    ),
    updated_StateName = case_when(
      # Gravelly mappings
      grepl("Gravelly", SiteName) & EcologicalState == 2.1 ~ "Shrub/Tree Dominant",
      grepl("Gravelly", SiteName) & EcologicalState == 2.2 ~ "Shrub/Tree Dominant",
      grepl("Gravelly", SiteName) & EcologicalState == 3.1 ~ "Shrubland or Woodland",
      grepl("Gravelly", SiteName) & EcologicalState == 4.1 ~ "Shrubland or Woodland",
      grepl("Gravelly", SiteName) & EcologicalState == 5.1 ~ "Bare (with or without annuals)",
      
      # Sandy mappings
      grepl("Sandy", SiteName) & EcologicalState == 1.1 ~ "Grassland (HCPC state)",
      grepl("Sandy", SiteName) & EcologicalState == 1.2 ~ "Grassland (HCPC state)",
      grepl("Sandy", SiteName) & EcologicalState == 2.1 ~ "Altered Grassland",
      grepl("Sandy", SiteName) & EcologicalState == 2.2 ~ "Altered Grassland",
      grepl("Sandy", SiteName) & EcologicalState == 3.1 ~ "Shrub/Tree-Invaded Grassland",
      grepl("Sandy", SiteName) & EcologicalState == 3.2 ~ "Shrub/Tree-Invaded Grassland",
      grepl("Sandy", SiteName) & EcologicalState == 4.1 ~ "Shrubland or Woodland",
      grepl("Sandy", SiteName) & EcologicalState == 5.1 ~ "Exotic Invaded",
      TRUE ~ NA_character_
    )
  )

# Filter out rows where updated_StateName is NA and exclude "Exotic Invaded"
filtered_aero_data <- filtered_aero_data %>%
  filter(!is.na(updated_StateName) & updated_StateName != "Exotic Invaded")

# Create the plot
ggplot(filtered_aero_data, aes(x = horizontal_flux_total_MD, y = SiteName, fill = SiteName)) +
  geom_boxplot() + # Use boxplot for visualization
  labs(
    title = "Horizontal Flux by Ecological Site",
    x = "Horizontal Flux Total (MD)",
    y = "Ecological Site"
  ) +
  theme_minimal()

# Export to CSV
write.csv(filtered_aero_data, "filtered_aero_data.csv", row.names = FALSE)

