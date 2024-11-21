#### SETUP ####

# Load necessary libraries
library(sf)          # Spatial data processing
library(tidyverse)   # Data manipulation and visualization
library(lubridate)   # Handling date/time data
library(ggplot2)     # Visualization
library(changepoint) # Change point detection
library(segmented)   # For piecewise regression analysis
library(car)         # Levene's Test

s# Load data
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
    y = "Ecological State"
  ) +
  geom_vline(xintercept = percentile_90, linetype = "dashed", color = "black", linewidth = 0.5) +
  theme_minimal()

#########################################################################
# 1. Benchmarks based on relationships between indicators

#### MODEL FITTING ####

# BARE SOIL COVER 
# Visualize the data

ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = horizontal_flux_total_MD, fill = updated_StateName, color = updated_StateName)) +
  geom_point() +
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

# Annotate with minimum radius and the x-value
text(x = min_curvature_x,
     y = max(sandy_df_filtered$horizontal_flux_total_MD) * 0.9,
     labels = paste("Min Radius: ", round(min_radius_of_curvature, 2), 
                    "\nX at Min Curvature: ", round(min_curvature_x, 2)),
     col = "red", cex = 1.2)

# Boxplot for Bare Soil Cover by ecological state
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, fill = SiteName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological State"
  ) +
  theme_minimal() +
  geom_vline(xintercept = min_curvature_x, linetype = "dashed", color = "black", size = 1)

# Boxplot for Bare Soil Cover by ecological state by points
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, color = SiteName)) +
  geom_point(alpha = 0.5, show.legend = FALSE, position = "jitter") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological State"
  ) +
  theme_minimal() +
  geom_vline(xintercept = min_curvature_x, linetype = "dashed", color = "black", size = 1)


### SEGMENTED (PIECEWISE) REGRESSION APPROACH ###

# Fit linear model as a base for segmented regression
base_model <- lm(horizontal_flux_total_MD ~ BareSoilCover, data = sandy_df_filtered)

# Fit segmented (piecewise) regression model
segmented_model <- segmented(base_model, seg.Z = ~ BareSoilCover, psi = 40) # Initial estimate of breakpoint at 40
summary(segmented_model)

# Extract the breakpoint and coefficients
breakpoint <- segmented_model$psi[2]
coefficients <- segmented_model$coefficients

# Add breakpoint line to the plot
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = horizontal_flux_total_MD, fill = updated_StateName, color = updated_StateName)) +
  geom_point() +
  geom_line(aes(y = predict(base_model), color = updated_StateName), linetype = "dotted") +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "red") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State with Breakpoint",
    x = "Bare Soil Cover (%)",
    y = "Total horizontal flux (g m-1 d-1)"
  ) +
  theme_minimal()

view(breakpoint)

# Boxplot for Bare Soil Cover by ecological state
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, fill = SiteName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological State"
  ) +
  theme_minimal() +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "black", size = 1)

# Boxplot for Bare Soil Cover by ecological state by points
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, color = SiteName)) +
  geom_point(alpha = 0.5, show.legend = FALSE, position = "jitter") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological Site"
  ) +
  theme_minimal() +
  geom_vline(xintercept = breakpoint, linetype = "dashed", color = "black", size = 1)


### VARIANCE BASED THRESHOLDS ###
# Define the ground cover indicator and Q
ground_cover_indicator <- sandy_df_filtered$BareSoilCover
Q <- sandy_df_filtered$horizontal_flux_total_MD

# Function to bin ground cover indicator based on its distribution
assign_bins <- function(data, indicator, n_bins = 10) {
  data %>%
    mutate(
      bin = cut(
        !!sym(indicator),
        breaks = quantile(!!sym(indicator), probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE),
        include.lowest = TRUE,
        labels = seq(1, n_bins)
      )
    )
}

# Bin the data
n_bins <- 10  # Set the number of bins
sandy_df_binned <- assign_bins(
  sandy_df_filtered,
  "BareSoilCover",
  n_bins = n_bins
)

# Calculate variance of Q for each bin
variance_by_bin <- sandy_df_binned %>%
  group_by(bin) %>%
  summarize(
    bin_center = mean(BareSoilCover, na.rm = TRUE),  # Calculate bin center for plotting
    Q_variance = var(horizontal_flux_total_MD, na.rm = TRUE),
    Q_count = n(),  # Check number of observations per bin
    .groups = "drop"
  )

# Visualize variance of Q by bin
ggplot(variance_by_bin, aes(x = bin_center, y = Q_variance)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Variance of Q by Ground Cover Bins",
    x = "Ground Cover Indicator (Bin Center)",
    y = "Variance of Q"
  ) +
  theme_minimal()

# Apply Levene's test to assess statistical changes in variance between bins
# Create a data frame to test for variance differences
levene_data <- sandy_df_binned %>%
  filter(!is.na(bin)) %>%
  mutate(bin = as.factor(bin))

levene_test_result <- leveneTest(horizontal_flux_total_MD ~ bin, data = levene_data)
print(levene_test_result)

# Identify breakpoints visually and statistically
variance_diff <- diff(variance_by_bin$Q_variance)
marked_increase_index <- which.max(variance_diff) + 1  # Bin after largest jump

benchmark_value <- max(variance_by_bin$bin_center[1:(marked_increase_index - 1)])

cat("The benchmark value of ground cover indicator is:", benchmark_value, "\n")

# Validate realistic erosion thresholds
# Plot raw data with the identified benchmark
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = horizontal_flux_total_MD)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = benchmark_value, linetype = "dashed", color = "red") +
  labs(
    title = "Horizontal Flux by Bare Soil Cover with Identified Threshold",
    x = "Bare Soil Cover (%)",
    y = "Total Horizontal Flux (g m^-1 d^-1)"
  ) +
  theme_minimal()

# Plot the actual data with the identified benchmark
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = horizontal_flux_total_MD)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = benchmark_value, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Horizontal Flux by Bare Soil Cover with Identified Threshold",
    subtitle = paste("Benchmark identified at Bare Soil Cover =", round(benchmark_value, 2)),
    x = "Bare Soil Cover (%)",
    y = "Total Horizontal Flux (g m^-1 d^-1)"
  ) +
  theme_minimal()

# Boxplot for Bare Soil Cover by ecological state
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, fill = SiteName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological State"
  ) +
  theme_minimal() +
  geom_vline(xintercept = benchmark_value, linetype = "dashed", color = "black", size = 1)

# Boxplot for Bare Soil Cover by ecological state by points
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, color = SiteName)) +
  geom_point(alpha = 0.5, show.legend = FALSE, position = "jitter") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological State"
  ) +
  theme_minimal() +
  geom_vline(xintercept = benchmark_value, linetype = "dashed", color = "black", size = 1)


#########################################################################
# 2. Benchmarks based on reference states and land potential

# Calculate quartiles for "Grassland (HCPC state)"
updated_StateName <- sandy_df_filtered %>%
  filter(updated_StateName == "Grassland (HCPC state)") %>%
  summarise(
    lower_quartile = quantile(BareSoilCover, 0.25, na.rm = TRUE),
    median_quartile = quantile(BareSoilCover, 0.50, na.rm = TRUE),
    upper_quartile = quantile(BareSoilCover, 0.75, na.rm = TRUE)
  )

# Extract quartile values
lower_quartile <- updated_StateName$lower_quartile
median_quartile <- updated_StateName$median_quartile
upper_quartile <- updated_StateName$upper_quartile

# Create the plot with added quantile lines
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, color = SiteName)) +
  geom_point(alpha = 0.5, show.legend = FALSE, position = "jitter") +
  scale_fill_brewer(palette = "Set1") +
  geom_vline(xintercept = lower_quartile, linetype = "dashed", color = "blue") +  # Lower quantile line
  geom_vline(xintercept = median_quartile, linetype = "solid", color = "red") +    # Median quantile line
  geom_vline(xintercept = upper_quartile, linetype = "dashed", color = "green") +  # Upper quantile line
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological State"
  ) +
  theme_minimal()


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

# Calculate the median of BareSoilCover
median_value <- median(sandy_df_filtered$BareSoilCover, na.rm = TRUE)

# Display the median value
print(paste("The median BareSoilCover is:", median_value))

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

# Boxplot for Bare Soil Cover by ecological state by boxplot
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, fill = SiteName)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological Site"
  ) +
  theme_minimal() +
  geom_vline(xintercept = median_value, linetype = "dashed", color = "black", size = 1)

# Boxplot for Bare Soil Cover by ecological state by points
ggplot(sandy_df_filtered, aes(x = BareSoilCover, y = updated_StateName, color = SiteName)) +
  geom_point(alpha = 0.5, show.legend = FALSE, position = "jitter") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil (%)",
    y = "Ecological State"
  ) +
  theme_minimal() +
  
  geom_vline(xintercept = median_value, linetype = "dashed", color = "black", size = 1)

#######################################################################
### MASTER INDICATOR TABLE ###

# Create the data frame with your reference values
bare_soil_table <- data.frame(
  Bare_Soil = c(min_curvature_x, breakpoint, benchmark_value, median_value, median_quartile),
  row.names = c("Min Curvature X", "Breakpoint", "Benchmark Value", "Median Value", "Reference State")
)

view(mean_Indicator <- mean(bare_soil_table$Bare_Soil))

# Define the bin range around the mean (± 25 points)
lower_bound <- mean_Indicator - (10/2)
upper_bound <- mean_Indicator + (10/2)

# Create the boxplot and add vertical lines for each reference value
ggplot(sandy_df_filtered, aes(x = BareSoilCover , y = horizontal_flux_total_MD, fill = updated_StateName, color = updated_StateName)) +
  geom_point() +  # Change from geom_point() to geom_boxplot() to create a boxplot
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil Cover (%)",
    y = "Total Horizontal Flux (g m^-1 d^-1)"
  ) +
  # Adding vertical lines for each reference value
  geom_vline(xintercept = min_curvature_x, linetype = "dashed", color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = breakpoint, linetype = "dotted", color = "green", linewidth = 0.5) +
  geom_vline(xintercept = benchmark_value, linetype = "solid", color = "red", linewidth = 0.5) +
  geom_vline(xintercept = median_value, linetype = "solid", color = "purple", linewidth = 0.5) +
  geom_vline(xintercept = median_quartile, linetype = "longdash", color = "orange", linewidth = 0.5) +
  theme_minimal()

# Create the boxplot and add vertical lines for the mean indicator
ggplot(sandy_df_filtered, aes(x = BareSoilCover , y = horizontal_flux_total_MD, fill = updated_StateName, color = updated_StateName)) +
  geom_point() +  # Change from geom_point() to geom_boxplot() to create a boxplot
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil Cover (%)",
    y = "Total Horizontal Flux (g m^-1 d^-1)"
  ) +
  # Adding vertical lines for each reference value
  geom_vline(xintercept = mean_Indicator, linetype = "solid", color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = lower_bound, linetype = "dashed", color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = upper_bound, linetype = "dashed", color = "blue", linewidth = 0.5) +
   theme_minimal()

# Create the boxplot and add vertical lines for the mean indicator
ggplot(sandy_df_filtered, aes(x = BareSoilCover , y = updated_StateName, fill = updated_StateName, color = updated_StateName)) +
  geom_boxplot() +  # Change from geom_point() to geom_boxplot() to create a boxplot
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Horizontal Flux by Ecological State",
    x = "Bare Soil Cover (%)",
    y = "Ecological State"
  ) +
  # Adding vertical lines for each reference value
  geom_vline(xintercept = mean_Indicator, linetype = "solid", color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = lower_bound, linetype = "dashed", color = "blue", linewidth = 0.5) +
  geom_vline(xintercept = upper_bound, linetype = "dashed", color = "blue", linewidth = 0.5) +
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

