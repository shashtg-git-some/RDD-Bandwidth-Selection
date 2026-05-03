# ==============================================================================
# FILE: 09_plot_cv.R
# PURPOSE: Visualize the Cross-Validation Objective Function
# ==============================================================================

library(ggplot2)

source("01_functions/01_dgp.R")
source("01_functions/04_bandwidth_cv.R")

if(!dir.exists("03_output")) dir.create("03_output")

cat("Generating CV Objective Plot (Using Case 3)...\n")

# 1. Generate a single dataset (Case 3 is good for this)
set.seed(123)

if (exists("generate_rdd_data")) {
  data <- generate_rdd_data(case = 3, N = 1000, tau = 1)
} else {
  stop("Error: Function 'generate_rdd_data' not found.")
}

# 2. Run CV on a grid
h_grid <- seq(0.1, 1.5, by = 0.05)
cv_errors <- numeric(length(h_grid))

for (i in seq_along(h_grid)) {
  res <- CV_bandwidth(data, bandwidth_grid = h_grid[i]) 
  cv_errors[i] <- res$cv_errors
}

# 3. Plot
df <- data.frame(h = h_grid, CV_Error = cv_errors)
best_h <- df$h[which.min(df$CV_Error)]

p <- ggplot(df, aes(x = h, y = CV_Error)) +
  geom_line(color = "darkblue", linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = best_h, color = "red", linetype = "dashed") +
  annotate("text", x = best_h + 0.3, y = max(df$CV_Error), 
           label = paste("Optimal h =", best_h), color = "red") +
  labs(title = "Cross-Validation Objective Function",
       subtitle = "The algorithm picks the bandwidth that minimizes prediction error",
       x = "Bandwidth (h)", y = "CV MSE Estimate") +
  theme_light()

ggsave("03_output/plot_cv_objective.pdf", plot = p, width = 7, height = 5)
cat("Saved: 03_output/plot_cv_objective.pdf\n")