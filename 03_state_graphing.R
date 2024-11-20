#### SETUP ####

# Load necessary libraries
library(sf)          # Spatial data processing
library(tidyverse)   # Data manipulation and visualization
library(lubridate)   # Handling date/time data
library(ggplot2)     # Visualization
library(changepoint) # Change point detection

# Load data
filtered_aero_data <- read.csv("filtered_aero_data.csv")

#### DATA PREPARATION ####

# Subset to only include rows where Site Name is "Sandy" and preprocess data
sandy_df <- filtered_aero_data %>%
  filter(SiteName == "Sandy") %>%
  mutate(SiteName = as.factor(updated_StateName))

# Filter and reorder data based on median horizontal flux
sandy_df_filtered <- sandy_df %>%
  mutate(updated_StateName = fct_reorder(updated_StateName, horizontal_flux_total_MD, .fun = median, .desc = TRUE))

# Calculate the 90th percentile for "Grassland (HCPC state)"
percentile_90 <- quantile(
  sandy_df_filtered %>%
    filter(updated_StateName == "Grassland (HCPC state)") %>%
    pull(horizontal_flux_total_MD),
  probs = 0.9,
  na.rm = TRUE
)

#### VISUALIZATION ####

# Boxplot for horizontal flux by ecological state
ggplot(sandy_df_filtered, aes(x = horizontal_flux_total_MD, y = updated_StateName, fill = updated_StateName, color = updated_StateName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  geom_point() +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Total horizontal flux (g m-1 d-1)",
    y = "Ecological Site"
  ) +
  geom_vline(xintercept = percentile_90, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme_minimal()

#########################################################################
# 1. Benchmarks based on relationships between indicators

#### MODEL FITTING ####

# BARE SOIL COVER 
# Visualize the data

ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = horizontal_flux_total_MD, fill = updated_StateName, color = updated_StateName)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil Cover (%)",
    y = "Total horizontal flux (g m-1 d-1)"
  ) +
  theme_minimal()

# Fit models
linear_model <- lm(horizontal_flux_total_MD ~ BareSoilCover, data = sandy_df_filtered)
poly_model <- lm(horizontal_flux_total_MD ~ poly(BareSoilCover, 2), data = sandy_df_filtered)
exp_model <- nls(horizontal_flux_total_MD ~ a * exp(b * BareSoilCover),
                 data = sandy_df_filtered,
                 start = list(a = 1, b = 0.1))

# Compare models using AIC
model_comparison <- AIC(linear_model, poly_model, exp_model)
print(model_comparison)

# Add residuals from the best-fit model (assume `exp_model` is best)
sandy_df_filtered$residuals <- residuals(exp_model)

#### EXPONENTIAL MODEL PLOT ####

# Plot the exponential model fit
plot(sandy_df_filtered$BareSoilCover, sandy_df_filtered$horizontal_flux_total_MD,
     main = "Exponential Fit: Bare Soil Cover vs Horizontal Flux Total MD",
     xlab = "Bare Soil Cover (%)",
     ylab = "Total horizontal flux (g m-1 d-1)",
     pch = 16, col = "black")

# Add model best-fit curves
smooth_x <- seq(min(sandy_df_filtered$BareSoilCover), max(sandy_df_filtered$BareSoilCover), length.out = 100)
lines(smooth_x, predict(exp_model, newdata = data.frame(BareSoilCover = smooth_x)), col = "green", lwd = 2)

# Annotate the plot with model equation and R-squared value
a_est <- coef(exp_model)["a"]
b_est <- coef(exp_model)["b"]
equation_text <- paste("y = ", round(a_est, 2), " * exp(", round(b_est, 2), " * x)", sep = "")
text(x = max(sandy_df_filtered$BareSoilCover) * 0.7,
     y = max(sandy_df_filtered$horizontal_flux_total_MD) * 0.8,
     labels = equation_text, col = "green", cex = 1.2)

#### CURVATURE ANALYSIS ####

# Define curvature calculation functions
curvature <- function(x) {
  dy_dx <- a_est * b_est * exp(b_est * x)
  d2y_dx2 <- a_est * b_est^2 * exp(b_est * x)
  (1 + dy_dx^2)^(3/2) / abs(d2y_dx2)
}

# Calculate curvature and identify minimum curvature
curvature_values <- sapply(smooth_x, curvature)
min_curvature <- min(curvature_values)
min_radius_of_curvature <- 1 / min_curvature
min_curvature_x <- smooth_x[which.min(curvature_values)]

# Add the minimum curvature to the plot
abline(v = min_curvature_x, col = "red", lty = 2)
text(x = min_curvature_x,
     y = max(sandy_df_filtered$horizontal_flux_total_MD) * 0.9,
     labels = paste("Min Radius: ", round(min_radius_of_curvature, 2)),
     col = "red", cex = 1.2)

### SEGMENTED REGRESSION APPROACH ###

### VARIANCE BASED THRESHOLDS ###


#########################################################################
# 2. Benchmarks based on reference states and land potential

# Calculate median, IQR, and quantiles for each State
result <- sandy_df_filtered %>%
  group_by(updated_StateName) %>%
  summarise(
    median_flux = median(horizontal_flux_total_MD, na.rm = TRUE),
    iqr_flux = IQR(horizontal_flux_total_MD, na.rm = TRUE),
    lower_quartile = quantile(horizontal_flux_total_MD, 0.25, na.rm = TRUE),
    upper_quartile = quantile(horizontal_flux_total_MD, 0.75, na.rm = TRUE)
  )

# View the result
print(result)

#### ADDITIONAL VISUALIZATION ####

# Boxplot for Bare Soil Cover by ecological state
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, fill = SiteName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological Site"
  ) +
  theme_minimal() +
  geom_vline(xintercept = min_curvature_x, linetype = "dashed", color = "black", size = 1)

#########################################################################
# 3. Benchmarks based on desired conditions from monitoring data and models

# Quantile benchmarks based on representing departure

# Add quartile categories to the data
sandy_df_filtered <- sandy_df_filtered %>%
  mutate(
    lower_quartile = quantile(BareSoilCover, 0.25, na.rm = TRUE),
    upper_quartile = quantile(BareSoilCover, 0.75, na.rm = TRUE),
    flux_category = case_when(
      BareSoilCover < lower_quartile ~ "Below 25th percentile",
      BareSoilCover > upper_quartile ~ "Above 75th percentile",
      TRUE ~ "Between 25th and 75th percentiles"
    )
  )

# Create the boxplot
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = factor(1))) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE, outlier.shape = NA) +
  geom_jitter(aes(color = updated_StateName), width = 0.2, size = 1.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1", name = "Quartile Range") +
  labs(
    title = "Bare Soil Cover Quartile Ranges",
    y = "Bare Soil Cover (%)",
    x = NULL
  ) +
  theme_minimal()







#######################################################################

### Gap Cover Analysis ###
# Calculate combined canopy gap for gaps > 100 cm
sandy_df_filtered <- sandy_df_filtered %>%
  mutate(GapCover_100_plus = GapCover_101_200 + GapCover_200_plus)

# Fit models with "GapCover_100_plus"
linear_model <- lm(horizontal_flux_total_MD ~ GapCover_100_plus, data = sandy_df_filtered)
poly_model <- lm(horizontal_flux_total_MD ~ poly(GapCover_100_plus, 2), data = sandy_df_filtered)
exp_model <- nls(horizontal_flux_total_MD ~ a * exp(b * GapCover_100_plus), 
                 data = sandy_df_filtered, 
                 start = list(a = 1, b = 0.1))

# Plot data with base R
plot(sandy_df_filtered$GapCover_100_plus, sandy_df_filtered$horizontal_flux_total_MD,
     main = "Exponential Fit: Gap Cover vs Horizontal Flux Total MD",
     xlab = "Gap Cover (%)",
     ylab = "Total horizontal flux (g m-1 d-1)",
     pch = 16, col = "black")

# Smooth x-axis range for fitted lines
smooth_x <- seq(min(sandy_df_filtered$GapCover_100_plus), max(sandy_df_filtered$GapCover_100_plus), length.out = 100)

# Add model best-fit curves
lines(smooth_x, predict(poly_model, newdata = data.frame(GapCover_100_plus = smooth_x)), col = "red", lwd = 2)
lines(smooth_x, predict(exp_model, newdata = data.frame(GapCover_100_plus = smooth_x)), col = "green", lwd = 2)

# Compare models using AIC
model_comparison <- AIC(linear_model, poly_model, exp_model)
print(model_comparison)

# Add residuals from the best-fit model (assume exp_model is best)
sandy_df_filtered$residuals <- residuals(exp_model)

### Minimum Curvature Analysis ###
# Exponential function and derivatives
exp_function <- function(x, a, b) {
  return(a * exp(b * x))
}

exp_derivative_1 <- function(x, a, b) {
  return(a * b * exp(b * x))
}

exp_derivative_2 <- function(x, a, b) {
  return(a * b^2 * exp(b * x))
}

# Extract coefficients
a_est <- coef(exp_model)["a"]
b_est <- coef(exp_model)["b"]

# Calculate curvature
curvature <- function(x) {
  dy_dx <- exp_derivative_1(x, a_est, b_est)
  d2y_dx2 <- exp_derivative_2(x, a_est, b_est)
  return((1 + dy_dx^2)^(3/2) / abs(d2y_dx2))
}

# Apply curvature function
curvature_values <- sapply(smooth_x, curvature)
min_curvature <- min(curvature_values)
min_radius_of_curvature <- 1 / min_curvature
min_curvature_x <- smooth_x[which.min(curvature_values)]

# Plot with curvature
plot(sandy_df_filtered$GapCover_100_plus, sandy_df_filtered$horizontal_flux_total_MD,
     main = "Exponential Fit: Gap Cover vs Horizontal Flux Total MD",
     xlab = "Gap Size (cm)",
     ylab = "Horizontal Flux Total MD (g m-1 d-1)",
     pch = 16, col = "black")

smooth_y_exp <- predict(exp_model, newdata = data.frame(GapCover_100_plus = smooth_x))
lines(smooth_x, smooth_y_exp, col = "green", lwd = 2)

# Add equation and R-squared
equation_text <- paste("y = ", round(a_est, 2), " * exp(", round(b_est, 2), " * x)", sep = "")
text(x = max(sandy_df_filtered$GapCover_100_plus) * 0.7, 
     y = max(sandy_df_filtered$horizontal_flux_total_MD) * 0.8, 
     labels = equation_text, col = "green", cex = 1.2)

abline(v = min_curvature_x, col = "red", lty = 2)
text(x = min_curvature_x, y = max(sandy_df_filtered$horizontal_flux_total_MD) * 0.9, 
     labels = paste("Min Radius: ", round(min_radius_of_curvature, 2)), 
     col = "red", cex = 1.2)

# Create the plot for Gap Cover
ggplot(sandy_df_filtered, aes(x = GapCover_100_plus, y = updated_StateName, fill = SiteName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Gap Size (cm)",
    y = "Ecological Site"
  ) +
  theme(legend.position = "none") +
  theme_minimal() +
  geom_vline(xintercept = min_curvature_x, linetype = "dashed", color = "black", size = 1)

#######################################################################

### Vegetation Height Analysis ###
# Calculate average vegetation height and plot by texture
sandy_df_filtered <- sandy_df_filtered %>%
  mutate(Hgt_Vegetation = rowMeans(select(., Hgt_Woody_Avg, Hgt_Herbaceous_Avg), na.rm = TRUE))

# Create the plot with RColorBrewer palette
ggplot(sandy_df_filtered, aes(x = Hgt_Vegetation, y = updated_StateName, fill = SiteName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) + 
  # geom_point(alpha = 0.5, show.legend = FALSE, aes(color = SiteName)) +
  scale_fill_brewer(palette = "Set1") +  # Use a predefined color palette +
  scale_color_brewer(palette = "Set1") +  # Use a predefined color palette
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Mean vegetation height (cm)",
    y = "Ecological Site"
  ) +
  theme(legend.position = "none") +
  theme_minimal()





# Examining GRAVEL ECOLOGICAL SITE
# Subset to only include rows where Site Name is "Gravelly"
gravel_df <- subset(filtered_aero_data, SiteName == "Gravelly")

# Filter out rows with non-positive values
gravel_df_filtered <- gravel_df %>%
  mutate(
    # Reorder updated_StateName by median horizontal_flux_total_MD
    updated_StateName = fct_reorder(updated_StateName, horizontal_flux_total_MD, .fun = median, .desc = TRUE)
  )

# Create the plot with RColorBrewer palette
ggplot(gravel_df_filtered, aes(x = horizontal_flux_total_MD, y = updated_StateName, fill = updated_StateName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) + 
  # geom_point(alpha = 0.5, show.legend = FALSE, aes(color = updated_StateName)) +
  scale_fill_brewer(palette = "Set2", n = 5) +  # Use a predefined color palette +
  scale_color_brewer(palette = "Set2", n = 5) +  # Use a predefined color palette
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Total horiztonal flux (g m-1 d-1)",
    y = "Ecological Site"
  ) +
  theme(legend.position = "none") +
  theme_minimal()


# Create the plot for Bare Soil Cover
# Create the plot with RColorBrewer palette
ggplot(gravel_df_filtered, aes(x = BareSoilCover, y = updated_StateName, fill = updated_StateName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) + 
  # geom_point(alpha = 0.5, show.legend = FALSE, aes(color = updated_StateName)) +
  scale_fill_brewer(palette = "Set1") +  # Use a predefined color palette +
  scale_color_brewer(palette = "Set1") +  # Use a predefined color palette
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological State"
  ) +
  theme(legend.position = "none") +
  theme_minimal()

### Gap Cover Analysis ###
# Calculate combined canopy gap for gaps > 100 cm
gravel_df_filtered <- gravel_df_filtered %>%
  mutate(GapCover_100_plus = GapCover_101_200 + GapCover_200_plus)

# Create the plot with RColorBrewer palette
ggplot(gravel_df_filtered, aes(x = GapCover_100_plus, y = updated_StateName, fill = updated_StateName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) + 
  # geom_point(alpha = 0.5, show.legend = FALSE, aes(color = updated_StateName)) +
  scale_fill_brewer(palette = "Set1") +  # Use a predefined color palette +
  scale_color_brewer(palette = "Set1") +  # Use a predefined color palette
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Canopy gap > 100 cm (%)",
    y = "Ecological State"
  ) +
  theme(legend.position = "none") +
  theme_minimal()


### Vegetation Height Analysis ###
# Calculate average vegetation height and plot by texture
gravel_df_filtered <- gravel_df_filtered %>%
  mutate(Hgt_Vegetation = rowMeans(select(., Hgt_Woody_Avg, Hgt_Herbaceous_Avg), na.rm = TRUE))

# Create the plot with RColorBrewer palette
ggplot(gravel_df_filtered, aes(x = Hgt_Vegetation, y = updated_StateName, fill = updated_StateName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) + 
  # geom_point(alpha = 0.5, show.legend = FALSE, aes(color = updated_StateName)) +
  scale_fill_brewer(palette = "Set1") +  # Use a predefined color palette +
  scale_color_brewer(palette = "Set1") +  # Use a predefined color palette
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Mean vegetation height (cm)",
    y = "Ecological State"
  ) +
  theme(legend.position = "none") +
  theme_minimal()

# NEXT STEPS: 
# STEP 1: CALCULATE THE CRITICAL THRESHOLDS OF INDICATORS

# 90th percentile of indicator values for the reference
# “major”, “moderate”, or “minimal” departure from reference conditions, respectively (Hughes et al., 1994; Stoddard et al., 2006).
# STEP 2: MAP BASED ON WHERE MEDIAN CATEGORY FALLS

